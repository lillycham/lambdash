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
  , prelude
  , lookupClass
  , lookupInstance
  , superclassesOf
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

-- | The classes and instances in scope.
data ClassEnv = ClassEnv
  { envClasses :: [ClassInfo]
  , envInstances :: [Instance]
  }

-- | The prelude's classes and instances.
prelude :: ClassEnv
prelude = ClassEnv preludeClasses preludeInstances

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
    <> [Instance "Eq" "List" [["Eq"]]]
    -- Structural: a record is Eq when every field is. See 'recordFields'.
    <> [Instance "Eq" "Record" []]
    <> [Instance "Ord" ty [] | ty <- ["Int", "Bool", "String", "()"]]
    <> [Instance "Ord" "List" [["Ord"]]]
    <> [Instance "Num" "Int" [], Instance "Integral" "Int" []]
    <> [Instance "Semigroup" ty [] | ty <- ["String", "Bytes", "Overlay"]]
    <> [Instance "Semigroup" "List" [[]]]
    <> [Instance "Show" ty [] | ty <- ["Int", "Bool", "String", "()", "Status", "Bytes", "Patch", "Overlay"]]
    <> [Instance "Show" "List" [["Show"]], Instance "Show" "Record" []]

-- | The constructor of a type, for instance lookup, and its arguments.
tyKey :: Ty -> Maybe (String, [Ty])
tyKey (TCon name) = Just (name, [])
tyKey (TList item) = Just ("List", [item])
tyKey (TFun a b) = Just ("->", [a, b])
tyKey _ = Nothing

lookupClass :: ClassEnv -> String -> Maybe ClassInfo
lookupClass env cls = find ((== cls) . className) (envClasses env)

lookupInstance :: ClassEnv -> String -> String -> Maybe Instance
lookupInstance env cls key = find (\i -> instClass i == cls && instHead i == key) (envInstances env)

-- | Every superclass, transitively.
superclassesOf :: ClassEnv -> String -> [String]
superclassesOf env cls = nub (concat [supers <> concatMap (superclassesOf env) supers | ClassInfo {className = name, classSupers = supers} <- envClasses env, name == cls])

-- | Declare a class. A class declared again replaces the old one, and its
-- instances go with it: they have the old methods.
addClass :: ClassInfo -> ClassEnv -> ClassEnv
addClass info env = ClassEnv
  (filter ((/= className info) . className) (envClasses env) <> [info])
  (filter ((/= className info) . instClass) (envInstances env))

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
