{-# LANGUAGE PatternSynonyms #-}

-- | Types, substitutions and unification: the first piece of the checker.
--
-- A substitution is a solution so far: what each unknown has turned out to be.
-- Unification takes two types and finds the substitution that makes them equal.
module Lambdash.Unify
  ( Var (..),
    Ty (..),
    pattern TInt,
    pattern TBool,
    pattern TUnit,
    pattern TString,
    pattern TStatus,
    pattern TBytes,
    Label,
    Row (..),
    Subst (..),
    Unifier,
    TypeError (..),
    emptySubst,
    singleton,
    singletonRow,
    freshVar,
    apply,
    applyRow,
    compose,
    occurs,
    occursRow,
    bind,
    bindRow,
    rewriteRow,
    unify,
    unifyRow,
  )
where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, put)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

-- | An unknown type. @infer@ will make fresh ones by counting.
newtype Var = Var Int
  deriving (Eq, Ord, Show)

data Ty
  = TVar Var
  | TFun Ty Ty
  | -- | A type with a name and no parts.
    TCon String
  | -- | A record with these fields.
    TRecord Row
  | -- | A computation that gives this type when it runs. It is a value until
    -- something runs it.
    TComp Ty
  | -- | A stream with an input item, output item and return value.
    TStream Ty Ty Ty
  deriving (Eq, Show)

-- | The name of a record field.
type Label = String

-- | The fields of a record: a list of fields with a tail. 'REmpty' ends a closed
-- record. 'RVar' is an unknown rest. An unknown is a 'Var' here too, so one counter
-- makes both, but it stands for a 'Row', not a 'Ty'.
data Row
  = REmpty
  | RExtend Label Ty Row
  | RVar Var
  deriving (Eq, Show)

pattern TInt, TBool, TUnit, TString, TStatus, TBytes :: Ty
pattern TInt = TCon "Int"
pattern TBool = TCon "Bool"
pattern TUnit = TCon "()"
pattern TString = TCon "String"
pattern TStatus = TCon "Status"
pattern TBytes = TCon "Bytes"

-- | Unknowns that stand for types, and unknowns that stand for rows.
data Subst = Subst {getSubst :: Map Var Ty, getRowSubst :: Map Var Row}
  deriving (Eq, Show)

data TypeError
  = -- | Two types can never be equal.
    Mismatch Ty Ty
  | -- | The solution for this unknown would be an infinite type.
    Infinite Var Ty
  | -- | The solution for this unknown would be an infinite row.
    InfiniteRow Var Row
  | -- | This row can never have this field.
    MissingField Label Row
  deriving (Eq, Show)

-- | The substitution that solves nothing.
emptySubst :: Subst
emptySubst = Subst Map.empty Map.empty

-- | One solved unknown.
singleton :: Var -> Ty -> Subst
singleton v t = Subst (Map.singleton v t) Map.empty

-- | One solved row unknown.
singletonRow :: Var -> Row -> Subst
singletonRow v r = Subst Map.empty (Map.singleton v r)

-- | Unification can grow a row, so it needs fresh unknowns too. This shares the
-- counter of @infer@. Use @lift@ to run an @Either TypeError a@ in here.
type Unifier = StateT Int (Either TypeError)

-- | Make an unknown that nothing has used yet.
freshVar :: Unifier Var
freshVar = do
  n <- get
  put (n + 1)
  pure (Var n)

-- | Replace each solved unknown in a type.
apply :: Subst -> Ty -> Ty
apply s (TVar v) = case Map.lookup v (getSubst s) of
  Nothing -> TVar v
  Just t -> t
apply s (TFun t1 t2) = TFun (apply s t1) (apply s t2)
apply s (TRecord r) = TRecord (applyRow s r)
apply s (TComp t) = TComp (apply s t)
apply s (TStream i o r) = TStream (apply s i) (apply s o) (apply s r)
apply _ t = t

-- | Replace each solved unknown in a row, in the tail and in the field types.
applyRow :: Subst -> Row -> Row
applyRow _ REmpty = REmpty
applyRow s (RVar v) = Map.findWithDefault (RVar v) v (getRowSubst s)
applyRow s (RExtend l t r) = RExtend l (apply s t) (applyRow s r)

-- | Compose like functions: @apply (compose s1 s2) = apply s1 . apply s2@.
-- So @s2@ happens first. Both maps need this, and each can mention the other.
compose :: Subst -> Subst -> Subst
compose s1 s2 = Subst
  { getSubst = Map.map (apply s1) (getSubst s2) `Map.union` getSubst s1
  , getRowSubst = Map.map (applyRow s1) (getRowSubst s2) `Map.union` getRowSubst s1
  }

-- | Does this unknown appear anywhere in this type?
occurs :: Var -> Ty -> Bool
occurs v (TVar v') = v == v'
occurs v (TFun t1 t2) = occurs v t1 || occurs v t2
occurs v (TRecord r) = occursRow v r
occurs v (TComp t) = occurs v t
occurs v (TStream i o r) = any (occurs v) [i, o, r]
occurs _ _ = False

-- | Does this unknown appear anywhere in this row?
occursRow :: Var -> Row -> Bool
occursRow _ REmpty = False
occursRow v (RVar v') = v == v'
occursRow v (RExtend _ t r) = occurs v t || occursRow v r

-- | Solve one unknown, or refuse.
bind :: Var -> Ty -> Either TypeError Subst
bind v t
  | t == TVar v = Right emptySubst
  | occurs v t = Left (Infinite v t)
  | otherwise = Right (singleton v t)

-- | Solve one row unknown, or refuse.
bindRow :: Var -> Row -> Either TypeError Subst
bindRow v r
  | r == RVar v = Right emptySubst
  | occursRow v r = Left (InfiniteRow v r)
  | otherwise = Right (singletonRow v r)

-- | Pull a field to the front. The result is the field's type, the row without
-- that field in its old order, and what that taught us.
--
-- If the tail is an unknown, grow it: the tail becomes the field, with a fresh
-- type, and a fresh rest.
--
-- The returned field type and remainder already include the returned
-- substitution. Callers must still apply it to types from outside this row.
rewriteRow :: Label -> Row -> Unifier (Ty, Row, Subst)
rewriteRow label REmpty = lift (Left (MissingField label REmpty))
rewriteRow label (RVar v) = do
  fieldTy <- TVar <$> freshVar
  tailRow <- RVar <$> freshVar
  sub <- lift (bindRow v (RExtend label fieldTy tailRow))
  pure (fieldTy, tailRow, sub)
rewriteRow label (RExtend otherLabel otherTy rest)
  | label == otherLabel = pure (otherTy, rest, emptySubst)
  | otherwise = do
      (fieldTy, restRow, sub) <- rewriteRow label rest
      pure (fieldTy, RExtend otherLabel (apply sub otherTy) restRow, sub)

-- | Find the substitution that makes two types equal.
--
-- Note: what you learn from one side of a 'TFun' has to apply to the other.
unify :: Ty -> Ty -> Unifier Subst
unify t1 t2 | t1 == t2 = pure emptySubst
unify (TVar v) t = lift (bind v t)
unify t (TVar v) = lift (bind v t)
unify (TFun l1 r1) (TFun l2 r2) = do
  s1 <- unify l1 l2
  s2 <- unify (apply s1 r1) (apply s1 r2)
  pure (s2 `compose` s1)
unify (TRecord r1) (TRecord r2) = unifyRow r1 r2
unify (TComp a) (TComp b) = unify a b
unify (TStream i o r) (TStream i' o' r') = do
  s1 <- unify i i'
  s2 <- unify (apply s1 o) (apply s1 o')
  let known = s2 `compose` s1
  s3 <- unify (apply known r) (apply known r')
  pure (s3 `compose` known)
unify t1 t2 = lift (Left (Mismatch t1 t2))

-- | Find the substitution that makes two rows equal, whatever the field order.
--
-- Note: if pulling a field out of the right row grew a tail, and the left row
-- has that same tail, then the rest can never finish. Refuse with 'InfiniteRow'.
unifyRow :: Row -> Row -> Unifier Subst
unifyRow REmpty REmpty = pure emptySubst
unifyRow (RVar v) r = lift (bindRow v r)
unifyRow r (RVar v) = lift (bindRow v r)
unifyRow REmpty (RExtend l _ _) = lift (Left (MissingField l REmpty))
unifyRow (RExtend label fieldTy leftRest) right = do
  (otherTy, rightRest, rewriteSub) <- rewriteRow label right
  checkSharedTail rewriteSub leftRest
  fieldSub <- unify (apply rewriteSub fieldTy) otherTy
  let known = fieldSub `compose` rewriteSub
  -- rewriteRow already applied rewriteSub to rightRest, but not to leftRest.
  restSub <- unifyRow (applyRow known leftRest) (applyRow fieldSub rightRest)
  pure (restSub `compose` known)
 where
  -- If rewriting grew the left tail too, recursion would grow it forever.
  checkSharedTail :: Subst -> Row -> Unifier ()
  checkSharedTail _ REmpty = pure ()
  checkSharedTail sub (RExtend _ _ rest) = checkSharedTail sub rest
  checkSharedTail sub (RVar v)
    | Map.member v (getRowSubst sub) =
        lift (Left (InfiniteRow v (applyRow sub leftRest)))
    | otherwise = pure ()
