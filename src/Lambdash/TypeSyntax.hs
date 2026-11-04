-- | Types as written, in signatures and annotations, as checker types.
module Lambdash.TypeSyntax
  ( Naming
  , sTy
  , unknown
  , writtenType
  , effectful
  , renderSType
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

effectful :: SType -> Bool
effectful written = case written of
  STApp name args -> name `elem` ["Comp", "Job", "Stream"] || any effectful args
  STList t -> effectful t
  STFun a b -> effectful a || effectful b
  STRecord fields _ -> any (effectful . snd) fields
  _ -> False

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
    STApp name args -> parens (level > 1) (unwords (name : map (go 2) args))
  parens True text = "(" <> text <> ")"
  parens False text = text
