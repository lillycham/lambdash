-- | Types as written, in signatures and annotations, as checker types.
module Lambdash.TypeSyntax
  ( Naming
  , sTy
  , methodTy
  , unknown
  , writtenType
  , renderSType
  , effectLabel
  ) where

import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import Data.List (intercalate)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Lambdash.Syntax
import Lambdash.Unify

-- | A written type, with an optional context, and every unknown in it: the
-- unknowns to bind. Each written name is one unknown; each computation, job
-- and stream has a row of its own.
writtenType :: [SPred] -> SType -> ([Var], Ty)
writtenType context written = evalState build (Map.empty, 0)
 where
  build = do
    ty <- sTy written
    preds <- mapM (\(SPred c t) -> IsIn c <$> sTy t) context
    let whole = if null preds then ty else TClass preds ty
        vars = Set.unions (ftvTy ty : map predVars preds)
    pure (Set.toList vars, whole)
  ftvTy t = predVars (IsIn "" t)

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

-- | What a row that is not written means.
data Omitted
  = -- | In a signature or an annotation: whatever the binding does.
    OpenRow
  | -- | In a method's type: nothing. A method's effects are a ceiling that
    -- its class declares, so that the preflight knows them.
    EmptyRow

-- | A type in a signature or an annotation.
sTy :: SType -> Naming Ty
sTy = sTyIn OpenRow

-- | A type in a class's method signature.
methodTy :: SType -> Naming Ty
methodTy = sTyIn EmptyRow

-- | A label as written and as printed: @exec git@, @console@, @env@, @fs@,
-- @cwd@.
effectLabel :: String -> Maybe Effect
effectLabel text = case words text of
  ["exec", name] -> Just (Exec name)
  ["console"] -> Just Console
  ["env"] -> Just Environment
  ["fs"] -> Just Filesystem
  ["cwd"] -> Just WorkingDirectory
  _ -> Nothing

sTyIn :: Omitted -> SType -> Naming Ty
sTyIn omitted written = case written of
  STVar name -> TVar <$> unknown name
  STCon name -> pure (TCon name)
  STList t -> TList <$> go t
  STFun a b -> TFun <$> go a <*> go b
  STRecord fields rest -> do
    tailRow <- maybe (pure REmpty) (fmap RVar . unknown) rest
    fieldTys <- mapM (\(label, t) -> (,) label <$> go t) fields
    pure (TRecord (foldr (\(label, t) r -> RExtend label t r) tailRow fieldTys))
  STApp "Comp" row [t] -> TComp <$> effects row <*> go t
  STApp "Job" row [t] -> TJob <$> effects row <*> go t
  STApp "Stream" row [i, o, r] -> TStream <$> effects row <*> go i <*> go o <*> go r
  STApp "Stage" _ [i, o] -> TStage <$> go i <*> go o
  STApp "Collector" _ [i, o] -> TCollector <$> go i <*> go o
  -- A declared type: Maybe Int. One with no arguments is a constructor.
  STApp name _ [] -> pure (TCon name)
  STApp name _ args -> TData name <$> mapM go args
 where
  go = sTyIn omitted
  effects Nothing = case omitted of
    OpenRow -> freshRow
    EmptyRow -> pure noEffects
  -- A row's tail shares no name with a type's unknown: e in {| e} and e
  -- in a -> e are different unknowns.
  effects (Just (SRow labels rest)) = do
    tails <- maybe (pure Set.empty) (fmap Set.singleton . unknown . ('!' :)) rest
    pure (EffRow (Set.fromList [l | Just l <- map effectLabel labels]) tails)

-- | A written type as text, as it was written.
renderSType :: SType -> String
renderSType = go 0
 where
  go :: Int -> SType -> String
  go level written = case written of
    STVar name -> name
    STCon name -> name
    STList t -> "[" <> go 0 t <> "]"
    STFun a b -> parens (level > 0) (go 1 a <> " -> " <> go 0 b)
    STRecord fields rest -> "{" <> intercalate ", " [label <> " : " <> go 0 t | (label, t) <- fields] <> maybe "" (" | " <>) rest <> "}"
    STApp name row args -> parens (level > 1) (unwords (name : maybe [] (pure . renderRow) row <> map (go 2) args))
  renderRow (SRow labels rest) = "{" <> intercalate ", " labels <> maybe "" (" | " <>) rest <> "}"
  parens True text = "(" <> text <> ")"
  parens False text = text
