-- | Class, instance and data declarations: checked against the classes and
-- types in scope, and turned into what the session holds.
--
-- A class gives its methods, each a field of the class's dictionary. An
-- instance gives a dictionary, the code that 'elaborateInstance' checks and
-- elaborates, evaluated where it can refer to itself.
module Lambdash.Declare
  ( Declared (..)
  , declare
  , toScheme
  ) where

import Control.Monad (unless, when)
import Control.Monad.Trans.State.Strict (evalState)
import Data.List (nub)
import Data.Map.Lazy qualified as Map.Lazy
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Lambdash.Classes
import Lambdash.Desugar (desugar)
import Lambdash.Eval (Scope, Value, constructorValue, eval, methodValue)
import Lambdash.Infer
import Lambdash.Syntax
import Lambdash.TypeSyntax
import Lambdash.Unify

-- | What a declaration adds to the session.
data Declared = Declared
  { declaredClasses :: ClassEnv
  , -- | New names: a class's methods, or an instance's dictionary.
    declaredTypes :: [(Name, Scheme)]
  , declaredValues :: [(Name, Value)]
  , -- | What the prompt says.
    declaredMessage :: String
  , -- | An instance's dictionary, as code, for @:elab@.
    declaredCode :: Maybe (Name, Expr)
  }

-- | Check a declaration against what the session has.
declare :: ClassEnv -> Env -> Scope -> Declaration -> Either InferError Declared
declare classes env values declaration = case declaration of
  DeclClass supers cls var signatures defaults -> do
    when (cls `elem` preludeNames) (bad (cls <> " is a prelude class"))
    let (classVariable, methods) = evalState (convert var signatures) (Map.empty, 0)
    mapM_ (\(SPred sup ty) -> do
      unless (ty == STVar var) (bad ("a superclass constrains the class's unknown, " <> var))
      known sup) supers
    case [m | (m, ty) <- methods, Set.notMember classVariable (ftv ty)] of
      m : _ -> bad (m <> " must mention " <> var)
      [] -> pure ()
    case [m | (i, (m, _)) <- zip [0 :: Int ..] methods, m `elem` map fst (take i methods)] of
      m : _ -> bad (m <> " is declared twice")
      [] -> pure ()
    case [m | (m, _) <- defaults, m `notElem` map fst methods] of
      m : _ -> bad (m <> " has a default but no signature")
      [] -> pure ()
    case [m | (i, (m, _)) <- zip [0 :: Int ..] defaults, m `elem` map fst (take i defaults)] of
      m : _ -> bad (m <> " has two defaults")
      [] -> pure ()
    let core = [(m, desugar e) | (m, e) <- defaults]
        info = ClassInfo cls (nub [sup | SPred sup _ <- supers]) classVariable methods core
        schemes = [(m, methodScheme cls classVariable ty) | (m, ty) <- methods]
        classes' = addClass info classes
        withMethods = foldr (uncurry Map.insert) env schemes
    -- Each default must have its method's type. An instance checks it again
    -- at its own type, as if it had written it.
    mapM_ (\(m, e) -> case lookup m methods of
      Just ty -> do
        let check = Forall (Set.toList (Set.insert classVariable (ftv ty))) (TClass [IsIn cls (TVar classVariable)] (TFun ty ty))
        _ <- inferWith classes' (Map.insert "%check" check withMethods) (EApp (EVar "%check") e)
        pure ()
      Nothing -> pure ()) core
    pure Declared
      { declaredClasses = classes'
      , declaredTypes = schemes
      , declaredValues = [(m, methodValue m) | (m, _) <- methods]
      , declaredMessage = "class " <> cls
      , declaredCode = Nothing
      }
  DeclData name params constructors -> do
    when (name `elem` builtinTypes) (bad (name <> " is a builtin type"))
    case [p | (i, p) <- zip [0 :: Int ..] params, p `elem` take i params] of
      p : _ -> bad ("the parameter " <> p <> " is named twice")
      [] -> pure ()
    let names = map fst constructors
    case [c | (i, c) <- zip [0 :: Int ..] names, c `elem` take i names] of
      c : _ -> bad (c <> " is declared twice")
      [] -> pure ()
    -- Every type a field names exists, with as many arguments as it takes.
    -- The type itself is known, so a field can be recursive.
    let arity t = if t == name then Just (length params) else
          if t `elem` builtinTypes then Just 0 else length . dataParams <$> lookupData classes t
    mapM_ (\(t, given) -> case arity t of
      Nothing -> bad ("no type " <> t)
      Just wanted | wanted /= given -> bad (t <> " takes " <> show wanted <> " arguments, not " <> show given)
      _ -> pure ()) (concatMap (concatMap namedTypes . snd) constructors)
    -- The parameters are the first unknowns, so they are Var 0, Var 1, ...
    -- An omitted row in a field is empty, as in a method's type.
    let (vars, fields) = evalState (do
          vs <- mapM unknown params
          fs <- mapM (\(c, tys) -> (,) c <$> mapM methodTy tys) constructors
          pure (vs, fs)) (Map.empty, 0)
    case [ty | (_, tys) <- fields, ty <- tys, not (ftv ty `Set.isSubsetOf` Set.fromList vars)] of
      _ : _ -> bad "a field mentions an unknown that is not a parameter of the type"
      [] -> pure ()
    let info = DataInfo name vars fields
        result = dataType name (map TVar vars)
    pure Declared
      { declaredClasses = addData info classes
      , declaredTypes = [(c, Forall vars (foldr TFun result tys)) | (c, tys) <- fields]
      , declaredValues = [(c, constructorValue c (length tys)) | (c, tys) <- fields]
      , declaredMessage = "data " <> unwords (name : params)
      , declaredCode = Nothing
      }
  DeclInstance requires cls headTy definitions -> do
    known cls
    headArgs <- case headTy of
      STCon _ -> pure []
      STList a -> pure [a]
      STFun a b -> pure [a, b]
      STApp _ Nothing args -> pure args
      _ -> bad "an instance is for a type constructor: Int, [a], a -> b, Maybe a"
    names <- mapM (\arg -> case arg of
      STVar v -> pure v
      _ -> bad "an instance's head takes distinct unknowns: [a], not [Int]") headArgs
    when (length (nub names) /= length names) (bad "an instance's head takes distinct unknowns: [a], not [Int]")
    mapM_ (\(SPred c ty) -> do
      known c
      case ty of
        STVar v | v `elem` names -> pure ()
        _ -> bad "a context constrains the head's unknowns") requires
    let headType = evalState (sTy headTy) (Map.empty, 0)
        key = maybe "" fst (tyKey headType)
        needs = [nub [c | SPred c (STVar v') <- requires, v' == v] | v <- names]
        inst = Instance cls key needs
        dictionaryName = instanceName cls key
        withInstance = addInstance inst classes
        written = [(methodField m, desugar e) | (m, e) <- definitions]
        -- A method left out takes the class's default, if it has one.
        code = written <> maybe [] (\info -> [d | d@(m, _) <- classDefaults info, m `notElem` map fst written]) (lookupClass classes cls)
    elaborated <- elaborateInstance withInstance env inst headType code
    -- Allowed, as the session is open to change, but said: a binding
    -- checked earlier keeps the dictionary it was given.
    let replaced = case lookupInstance prelude cls key of
          Just _ -> "\nwarning: this replaces the builtin instance " <> cls <> " " <> renderHead headTy
            <> " from here on; earlier bindings that chose it keep the builtin one"
          Nothing -> ""
    let value = eval (Map.Lazy.insert dictionaryName value values) elaborated
    pure Declared
      { declaredClasses = withInstance
      , declaredTypes = []
      , declaredValues = [(dictionaryName, value)]
      , declaredMessage = "instance " <> cls <> " " <> renderHead headTy <> replaced
      , declaredCode = Just (dictionaryName, elaborated)
      }
 where
  bad = Left . BadDeclaration
  known c = maybe (bad ("no class " <> c)) (const (pure ())) (lookupClass classes c)
  preludeNames = map className (envClasses prelude)
  builtinTypes = ["Int", "Bool", "()", "String", "Status", "Bytes", "Overlay", "Patch", "Comp", "Job", "Stream", "Stage", "Collector"]
  -- The named types in a field, and how many arguments each has.
  namedTypes written = case written of
    STCon t -> [(t, 0)]
    STApp t _ args | t `notElem` ["Comp", "Job", "Stream", "Stage", "Collector"] -> (t, length args) : concatMap namedTypes args
    STApp _ _ args -> concatMap namedTypes args
    STList t -> namedTypes t
    STFun a b -> namedTypes a <> namedTypes b
    STRecord fs _ -> concatMap (namedTypes . snd) fs
    STVar _ -> []
  convert var signatures = do
    v <- unknown var
    methods <- mapM (\(m, ty) -> (,) m <$> methodTy ty) signatures
    pure (v, methods)
  renderHead (STCon name) = name
  renderHead (STList (STVar a)) = "[" <> a <> "]"
  renderHead (STFun (STVar a) (STVar b)) = "(" <> a <> " -> " <> b <> ")"
  renderHead (STApp con _ args) = "(" <> unwords (con : [a | STVar a <- args]) <> ")"
  renderHead _ = "?"

-- | A method's type: every unknown bound, and the class on its unknown.
methodScheme :: String -> Var -> Ty -> Scheme
methodScheme cls var ty =
  Forall (Set.toList (Set.insert var (ftv ty))) (TClass [IsIn cls (TVar var)] ty)

-- | A written type as a scheme over all its unknowns.
toScheme :: SType -> Scheme
toScheme written = uncurry Forall (writtenType [] written)
