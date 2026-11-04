-- | Class and instance declarations: checked against the classes in scope,
-- and turned into what the session holds.
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
import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import Data.List (nub)
import Data.Map.Lazy qualified as Map.Lazy
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Lambdash.Classes
import Lambdash.Desugar (desugar)
import Lambdash.Eval (Scope, Value, eval, methodValue)
import Lambdash.Infer
import Lambdash.Syntax
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
    -- A row in a signature would be unchecked: there is no syntax for one.
    case [m | (m, ty) <- signatures, effectful ty] of
      m : _ -> bad (m <> ": a method's type cannot mention Comp, Job or Stream yet, because their effect rows have no syntax")
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
  DeclInstance requires cls headTy definitions -> do
    known cls
    headArgs <- case headTy of
      STCon _ -> pure []
      STList a -> pure [a]
      STFun a b -> pure [a, b]
      _ -> bad "an instance is for a type constructor: Int, [a], a -> b"
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
    let value = eval (Map.Lazy.insert dictionaryName value values) elaborated
    pure Declared
      { declaredClasses = withInstance
      , declaredTypes = []
      , declaredValues = [(dictionaryName, value)]
      , declaredMessage = "instance " <> cls <> " " <> renderHead headTy
      , declaredCode = Just (dictionaryName, elaborated)
      }
 where
  bad = Left . BadDeclaration
  known c = maybe (bad ("no class " <> c)) (const (pure ())) (lookupClass classes c)
  preludeNames = map className (envClasses prelude)
  convert var signatures = do
    v <- unknown var
    methods <- mapM (\(m, ty) -> (,) m <$> sTy ty) signatures
    pure (v, methods)
  renderHead (STCon name) = name
  renderHead (STList (STVar a)) = "[" <> a <> "]"
  renderHead (STFun (STVar a) (STVar b)) = "(" <> a <> " -> " <> b <> ")"
  renderHead _ = "?"

effectful :: SType -> Bool
effectful written = case written of
  STApp name args -> name `elem` ["Comp", "Job", "Stream"] || any effectful args
  STList t -> effectful t
  STFun a b -> effectful a || effectful b
  STRecord fields _ -> any (effectful . snd) fields
  _ -> False

-- | A method's type: every unknown bound, and the class on its unknown.
methodScheme :: String -> Var -> Ty -> Scheme
methodScheme cls var ty =
  Forall (Set.toList (Set.insert var (ftv ty))) (TClass [IsIn cls (TVar var)] ty)

-- | A written type as a scheme over all its unknowns.
toScheme :: SType -> Scheme
toScheme written = let ty = evalState (sTy written) (Map.empty, 0) in Forall (Set.toList (ftv ty)) ty

type Naming = State (Map.Map Name Var, Int)

unknown :: Name -> Naming Var
unknown name = do
  (names, next) <- get
  case Map.lookup name names of
    Just v -> pure v
    Nothing -> do
      put (Map.insert name (Var next) names, next + 1)
      pure (Var next)

-- | A row of its own for each computation, job and stream.
freshRow :: Naming EffRow
freshRow = do
  (names, next) <- get
  put (names, next + 1)
  pure (effectVar (Var next))

sTy :: SType -> Naming Ty
sTy written = case written of
  STVar name -> TVar <$> unknown name
  STCon name -> pure (TCon name)
  STList t -> TList <$> sTy t
  STFun a b -> TFun <$> sTy a <*> sTy b
  STRecord fields rest -> do
    tailRow <- maybe (pure REmpty) (fmap RVar . unknown) rest
    fieldTys <- mapM (\(label, t) -> (,) label <$> sTy t) fields
    pure (TRecord (foldr (\(label, t) r -> RExtend label t r) tailRow fieldTys))
  STApp "Comp" [t] -> TComp <$> freshRow <*> sTy t
  STApp "Job" [t] -> TJob <$> freshRow <*> sTy t
  STApp "Stream" [i, o, r] -> TStream <$> freshRow <*> sTy i <*> sTy o <*> sTy r
  STApp "Stage" [i, o] -> TStage <$> sTy i <*> sTy o
  STApp "Collector" [i, o] -> TCollector <$> sTy i <*> sTy o
  STApp name _ -> pure (TCon name)
