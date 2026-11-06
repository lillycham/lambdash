-- | The classes and instances that the checker resolves against.
--
-- A 'ClassEnv' is data, so that declarations at the prompt extend it. The
-- prelude's is 'prelude'. The dictionaries themselves are values, under the
-- names that 'instanceName' gives: the prelude's are in "Lambdash.Eval", and
-- a declared instance's is the code its declaration elaborates to.
module Lambdash.Classes
  ( ClassInfo (..)
  , Instance (..)
  , ClassEnv (..)
  , DataInfo (..)
  , addData
  , lookupData
  , lookupConstructor
  , dataType
  , prelude
  , lookupClass
  , lookupInstance
  , superclassesOf
  , superclassPath
  , superclassCycle
  , addClass
  , addInstance
  , methodField
  , instanceName
  , tyKey
  , superField
  , recordFields
  , extendFields
  , fromIntName
  , defaultCandidates
  ) where

import Data.List (find, nub)
import Data.Set qualified as Set
import Lambdash.Syntax (Expr (..))
import Lambdash.Unify

data ClassInfo = ClassInfo
  { className :: String
  , -- | Direct superclasses. A dictionary holds one for each, so @Ord a@
    -- provides @Eq a@.
    classSupers :: [String]
  , -- | The unknown that stands for the instance type in 'classMethods'.
    classVar :: Var
  , -- | Each method's field in the dictionary, and its type. Other unknowns
    -- in a method's type are its own, chosen at each use.
    classMethods :: [(String, Ty)]
  , -- | Default definitions, in core: an instance that leaves a method out
    -- gets its default, checked as if the instance had written it.
    classDefaults :: [(String, Expr)]
  }

-- | An instance for a type constructor. Each argument of the constructor
-- must be an instance of the listed classes: @Eq [a]@ requires @Eq a@.
data Instance = Instance
  { instClass :: String
  , instHead :: String
  , instRequires :: [[String]]
  }

-- | The classes, instances and data types in scope. Data types are here
-- because the checker reads them as it reads classes: for patterns, and
-- for whether a case covers every value.
data ClassEnv = ClassEnv
  { envClasses :: [ClassInfo]
  , envInstances :: [Instance]
  , envData :: [DataInfo]
  }

-- | A declared data type: its parameters, and each constructor with the
-- types of its fields, which mention only the parameters.
data DataInfo = DataInfo
  { dataName :: String
  , dataParams :: [Var]
  , dataConstructors :: [(String, [Ty])]
  }

-- | The prelude's classes and instances.
prelude :: ClassEnv
prelude = ClassEnv preludeClasses preludeInstances []

preludeClasses :: [ClassInfo]
preludeClasses =
  [ ClassInfo "Eq" [] a [("eq", a2 TBool), ("ne", a2 TBool)]
      [("ne", binary (call "%not" [call "%eq" [x, y]]))]
  , ClassInfo "Ord" ["Eq"] a ([(m, a2 TBool) | m <- ["lt", "le", "gt", "ge"]] <> [(m, a2 ta) | m <- ["min", "max"]])
      [ ("le", binary (call "%not" [call "%lt" [y, x]]))
      , ("gt", binary (call "%lt" [y, x]))
      , ("ge", binary (call "%not" [call "%lt" [x, y]]))
      , ("min", binary (call "%if" [call "%lt" [y, x], y, x]))
      , ("max", binary (call "%if" [call "%lt" [x, y], y, x])) ]
  , ClassInfo "Num" [] a ([(m, a2 ta) | m <- ["+", "-", "*"]] <> [("negate", TFun ta ta), ("fromInt", TFun TInt ta)]) []
  , ClassInfo "Integral" ["Num"] a [(m, a2 ta) | m <- ["/", "%"]] []
  , ClassInfo "Semigroup" [] a [("<>", a2 ta)] []
  , ClassInfo "Show" [] a [("show", TFun ta TString)] []
  , ClassInfo "Display" [] a [("display", TFun ta TString)] []
  ]
 where
  a = Var 0
  ta = TVar a
  a2 result = TFun ta (TFun ta result)
  -- The prelude's defaults use names that no session can rebind.
  x = EVar "%x"
  y = EVar "%y"
  binary = ELam "%x" . ELam "%y"
  call f = foldl EApp (EVar f)

preludeInstances :: [Instance]
preludeInstances =
  [Instance "Eq" ty [] | ty <- ["Int", "Bool", "String", "()", "Status", "Bytes", "Patch", "Overlay"]]
    <> [Instance "Eq" "[]" [["Eq"]]]
    -- Structural: a record is Eq when every field is. See 'recordFields'.
    <> [Instance "Eq" "{}" []]
    <> [Instance "Ord" ty [] | ty <- ["Int", "Bool", "String", "()"]]
    <> [Instance "Ord" "[]" [["Ord"]]]
    <> [Instance "Num" "Int" [], Instance "Integral" "Int" []]
    <> [Instance "Semigroup" ty [] | ty <- ["String", "Bytes", "Overlay"]]
    <> [Instance "Semigroup" "[]" [[]]]
    <> [Instance "Show" ty [] | ty <- ["Int", "Bool", "String", "()", "Status", "Bytes", "Patch", "Overlay"]]
    <> [Instance "Show" "[]" [["Show"]], Instance "Show" "{}" []]
    <> [Instance "Display" ty [] | ty <- ["Int", "Bool", "String", "()", "Status", "Bytes", "Patch", "Overlay"]]
    <> [Instance "Display" "[]" [["Display"]], Instance "Display" "{}" []]

-- | The constructor of a type, for instance lookup, and its arguments.
tyKey :: Ty -> Maybe (String, [Ty])
tyKey (TCon name) = Just (name, [])
tyKey (TList item) = Just ("[]", [item])
tyKey (TData name args) = Just (name, args)
tyKey (TFun a b) = Just ("->", [a, b])
tyKey _ = Nothing

lookupClass :: ClassEnv -> String -> Maybe ClassInfo
lookupClass env cls = find ((== cls) . className) (envClasses env)

lookupInstance :: ClassEnv -> String -> String -> Maybe Instance
lookupInstance env cls key = find (\i -> instClass i == cls && instHead i == key) (envInstances env)

-- | A class's own superclasses, as declared.
directSupers :: ClassEnv -> String -> [String]
directSupers env cls = concat [supers | ClassInfo {className = name, classSupers = supers} <- envClasses env, name == cls]

-- | Every superclass, transitively, nearest first. Each class shows once,
-- so a cycle cannot make this loop.
superclassesOf :: ClassEnv -> String -> [String]
superclassesOf env cls = go Set.empty (directSupers env cls)
 where
  go _ [] = []
  go seen (next : rest)
    | Set.member next seen = go seen rest
    | otherwise = next : go (Set.insert next seen) (rest <> directSupers env next)

-- | The superclasses from one class up to another, the last being the
-- other: one step at least, so a class reaches itself only through a
-- cycle. A shortest path, found breadth first. A subclass's dictionary
-- holds its superclasses' dictionaries along this path.
superclassPath :: ClassEnv -> String -> String -> Maybe [String]
superclassPath env from to = go Set.empty [[s] | s <- directSupers env from]
 where
  go _ [] = Nothing
  go seen (path : rest) = case path of
    [] -> go seen rest
    next : _
      | next == to -> Just (reverse path)
      | Set.member next seen -> go seen rest
      | otherwise -> go (Set.insert next seen) (rest <> [s : path | s <- directSupers env next])

-- | A class that is its own superclass, and the path back to it, as in
-- @[C, D, C]@.
superclassCycle :: ClassEnv -> Maybe [String]
superclassCycle env = case [cls : path | ClassInfo {className = cls} <- envClasses env, Just path <- [superclassPath env cls cls]] of
  found : _ -> Just found
  [] -> Nothing

-- | Declare a class. A class declared again replaces the old one, and its
-- instances go with it: they have the old methods.
addClass :: ClassInfo -> ClassEnv -> ClassEnv
addClass info env = env
  { envClasses = filter ((/= className info) . className) (envClasses env) <> [info]
  , envInstances = filter ((/= className info) . instClass) (envInstances env)
  }

-- | Declare an instance. One for the same class and type replaces it, for
-- the lines after.
addInstance :: Instance -> ClassEnv -> ClassEnv
addInstance inst env = env
  { envInstances = filter (\i -> not (instClass i == instClass inst && instHead i == instHead inst)) (envInstances env) <> [inst] }

-- | The dictionary field of a method, as an instance may name it: @(==)@
-- is the prelude's @eq@, and @div@ is @/@.
methodField :: String -> String
methodField name = maybe name id (lookup name aliases)
 where
  aliases =
    [ ("==", "eq"), ("!=", "ne"), ("<", "lt"), ("<=", "le"), (">", "gt"), (">=", "ge")
    , ("div", "/"), ("mod", "%") ]

-- | The name of an instance's dictionary. No identifier can spell it.
instanceName :: String -> String -> String
instanceName cls key = "%instance " <> cls <> " " <> key

-- | The field of a dictionary that holds the dictionary of a superclass.
superField :: String -> String
superField cls = '%' : cls

-- | The field of a record's dictionary that holds the dictionaries of its
-- fields, by label. A function that is Eq over an open row, @Eq { | r}@,
-- takes the dictionary for the tail and extends its fields with the ones it
-- knows.
recordFields :: String
recordFields = "%fields"

-- | What an integer literal elaborates through: @1@ is @fromInt d 1@, with
-- @d@ the dictionary for @Num a@.
fromIntName :: String
fromIntName = "%fromInt"

-- | What an ambiguous unknown can default to, in order: the first type that
-- every class on it has an instance for. @eq [] []@ takes @()@; @eq 1 1@
-- takes @Int@, because @()@ is not @Num@.
defaultCandidates :: [Ty]
defaultCandidates = [TUnit, TInt]

-- | The builtin that extends a record of field dictionaries with more.
extendFields :: String
extendFields = "%extendFields"

-- | Declare a data type. One declared again replaces the old one, and the
-- instances for it go: they were for the old constructors.
addData :: DataInfo -> ClassEnv -> ClassEnv
addData info env = env
  { envData = filter ((/= dataName info) . dataName) (envData env) <> [info]
  , envInstances = filter ((/= dataName info) . instHead) (envInstances env)
  }

lookupData :: ClassEnv -> String -> Maybe DataInfo
lookupData env name = find ((== name) . dataName) (envData env)

-- | The data type a constructor belongs to, and the types of its fields.
-- A later declaration wins, as it does for names.
lookupConstructor :: ClassEnv -> String -> Maybe (DataInfo, [Ty])
lookupConstructor env name =
  case [(info, fields) | info <- reverse (envData env), (c, fields) <- dataConstructors info, c == name] of
    found : _ -> Just found
    [] -> Nothing

-- | The type a data type's values have, at these arguments.
dataType :: String -> [Ty] -> Ty
dataType name [] = TCon name
dataType name args = TData name args
