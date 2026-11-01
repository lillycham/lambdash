{-# LANGUAGE PatternSynonyms #-}

-- | Types, substitutions and unification: the first piece of the checker.
--
-- A substitution is a solution so far: what each unknown has turned out to be.
-- Unification takes two types and finds the substitution that makes them equal.
module Lambdash.Unify
  ( Var (..),
    Ty (..),
    Effect (..),
    EffRow (..),
    noEffects,
    effect,
    effectVar,
    unionEffects,
    applyEffects,
    unifyEffects,
    singletonEffect,
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
import Data.Set (Set)
import Data.Set qualified as Set

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
    TComp EffRow Ty
  | -- | A stream with an input item, output item and return value.
    TStream EffRow Ty Ty Ty
  deriving (Eq, Show)

data Effect = Exec String | Console deriving (Eq, Ord, Show)

-- | A finite set of labels and a union of open effect rows. Multiple tails
-- preserve independent requirements when higher-order streams compose.
data EffRow = EffRow (Set Effect) (Set Var) deriving (Eq, Show)

noEffects :: EffRow
noEffects = EffRow Set.empty Set.empty

effect :: Effect -> EffRow
effect label = EffRow (Set.singleton label) Set.empty

effectVar :: Var -> EffRow
effectVar var = EffRow Set.empty (Set.singleton var)

unionEffects :: EffRow -> EffRow -> EffRow
unionEffects (EffRow labels vars) (EffRow labels' vars') =
  EffRow (Set.union labels labels') (Set.union vars vars')

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
data Subst = Subst
  { getSubst :: Map Var Ty
  , getRowSubst :: Map Var Row
  , getEffectSubst :: Map Var EffRow
  }
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
  | EffectMismatch EffRow EffRow
  | UnresolvedEffects EffRow EffRow
  deriving (Eq, Show)

-- | The substitution that solves nothing.
emptySubst :: Subst
emptySubst = Subst Map.empty Map.empty Map.empty

-- | One solved unknown.
singleton :: Var -> Ty -> Subst
singleton v t = emptySubst {getSubst = Map.singleton v t}

-- | One solved row unknown.
singletonRow :: Var -> Row -> Subst
singletonRow v r = emptySubst {getRowSubst = Map.singleton v r}

singletonEffect :: Var -> EffRow -> Subst
singletonEffect v row = emptySubst {getEffectSubst = Map.singleton v row}

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
apply s (TComp eff t) = TComp (applyEffects s eff) (apply s t)
apply s (TStream eff i o r) = TStream (applyEffects s eff) (apply s i) (apply s o) (apply s r)
apply _ t = t

-- | Replace each solved unknown in a row, in the tail and in the field types.
applyRow :: Subst -> Row -> Row
applyRow _ REmpty = REmpty
applyRow s (RVar v) = Map.findWithDefault (RVar v) v (getRowSubst s)
applyRow s (RExtend l t r) = RExtend l (apply s t) (applyRow s r)

applyEffects :: Subst -> EffRow -> EffRow
applyEffects sub (EffRow labels vars) = foldr unionEffects (EffRow labels Set.empty)
  [Map.findWithDefault (effectVar var) var (getEffectSubst sub) | var <- Set.toList vars]

-- | Compose like functions: @apply (compose s1 s2) = apply s1 . apply s2@.
-- So @s2@ happens first. All three maps need this: types can contain either
-- kind of row, and record fields can contain types with effect rows.
compose :: Subst -> Subst -> Subst
compose s1 s2 = Subst
  { getSubst = Map.map (apply s1) (getSubst s2) `Map.union` getSubst s1
  , getRowSubst = Map.map (applyRow s1) (getRowSubst s2) `Map.union` getRowSubst s1
  , getEffectSubst = Map.map (applyEffects s1) (getEffectSubst s2) `Map.union` getEffectSubst s1
  }

-- | Does this unknown appear anywhere in this type?
occurs :: Var -> Ty -> Bool
occurs v (TVar v') = v == v'
occurs v (TFun t1 t2) = occurs v t1 || occurs v t2
occurs v (TRecord r) = occursRow v r
occurs v (TComp (EffRow _ vars) t) = Set.member v vars || occurs v t
occurs v (TStream (EffRow _ vars) i o r) = Set.member v vars || any (occurs v) [i, o, r]
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
unify (TComp eff a) (TComp eff' b) = do
  s1 <- unify a b
  s2 <- unifyEffects (applyEffects s1 eff) (applyEffects s1 eff')
  pure (s2 `compose` s1)
unify (TStream eff i o r) (TStream eff' i' o' r') = do
  s1 <- unify i i'
  s2 <- unify (apply s1 o) (apply s1 o')
  let known = s2 `compose` s1
  s3 <- unify (apply known r) (apply known r')
  let types = s3 `compose` known
  s4 <- unifyEffects (applyEffects types eff) (applyEffects types eff')
  pure (s4 `compose` types)
unify t1 t2 = lift (Left (Mismatch t1 t2))

-- | Solve exact effect-row equality. Unions are idempotent. A bare unknown
-- can take any row; self-absorption introduces an open residual. More general
-- set equalities require qualified constraints, which this slice diagnoses.
unifyEffects :: EffRow -> EffRow -> Unifier Subst
unifyEffects left right | left == right = pure emptySubst
unifyEffects left@(EffRow labels vars) right@(EffRow labels' vars')
  | Set.null labels, [var] <- Set.toList vars = bindEffect var right
  | Set.null labels', [var] <- Set.toList vars' = bindEffect var left
  | Set.null vars && Set.null vars' = lift (Left (EffectMismatch left right))
  | vars == vars', [var] <- Set.toList vars = do
      -- Common fixed labels mask optional membership in the tail. Only the
      -- symmetric difference must be present in it.
      let required = Set.union (labels Set.\\ labels') (labels' Set.\\ labels)
      tailVar <- freshVar
      pure (singletonEffect var (EffRow required (Set.singleton tailVar)))
  | otherwise = lift (Left (UnresolvedEffects left right))
 where
  bindEffect var row@(EffRow fixed tails)
    | Set.member var tails = do
        residual <- freshVar
        pure (singletonEffect var (EffRow fixed (Set.insert residual (Set.delete var tails))))
    | otherwise = pure (singletonEffect var row)

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
