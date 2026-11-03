{-# LANGUAGE PatternSynonyms #-}

-- | Types, substitutions and unification: the first piece of the checker.
--
-- A substitution is a solution so far: what each unknown has turned out to be.
-- Unification returns a substitution and any deferred effect-row equalities.
-- 'solveConstraints' checks their joint consistency before a type is used.
module Lambdash.Unify
  ( Var (..),
    Ty (..),
    Effect (..),
    EffRow (..),
    EffConstraint (..),
    qualify,
    unqualify,
    constraintVars,
    applyConstraint,
    solveConstraints,
    satisfiableEffects,
    closeEffects,
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
    pattern TOverlay,
    pattern TPatch,
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
import Data.List (nub)

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
  | -- | A running job, started by @&@, that gives this type when it ends. Its
    -- effect row travels with it.
    TJob EffRow Ty
  | -- | Equalities that must hold when the type is instantiated.
    TQualified [EffConstraint] Ty
  | TList Ty
  | -- | Pure adapters and collectors. Neither runs until connected.
    TStage Ty Ty
  | TCollector Ty Ty
  deriving (Eq, Show)

-- | @Environment@ is reading or changing the session's environments: @use@,
-- @unuse@ and @environments@.
-- @Filesystem@ is reading the filesystem, and @WorkingDirectory@ is changing
-- the current directory. Both are provisional, and the preflight ignores them.
data Effect = Exec String | Console | Environment | Filesystem | WorkingDirectory deriving (Eq, Ord, Show)

-- | A finite set of labels and a union of open effect rows. Multiple tails
-- preserve independent requirements when higher-order streams compose.
data EffRow = EffRow (Set Effect) (Set Var) deriving (Eq, Ord, Show)

data EffConstraint = EffEqual EffRow EffRow deriving (Eq, Ord, Show)

constraintVars :: EffConstraint -> Set Var
constraintVars (EffEqual (EffRow _ a) (EffRow _ b)) = Set.union a b

-- Qualifications live at the outside of a type, including those obtained
-- from a record field or function result.
qualify :: [EffConstraint] -> Ty -> Ty
qualify constraints ty = case nub (constraints <> nested) of
  [] -> bare
  pending -> TQualified pending bare
 where
  (nested, bare) = unqualify ty

unqualify :: Ty -> ([EffConstraint], Ty)
unqualify (TQualified constraints ty) = let (more, bare) = unqualify ty in (constraints <> more, bare)
unqualify (TFun a b) = let (ca, a') = unqualify a; (cb, b') = unqualify b in (ca <> cb, TFun a' b')
unqualify (TComp eff ty) = let (cs, bare) = unqualify ty in (cs, TComp eff bare)
unqualify (TJob eff ty) = let (cs, bare) = unqualify ty in (cs, TJob eff bare)
unqualify (TList ty) = let (cs, bare) = unqualify ty in (cs, TList bare)
unqualify (TStage i o) = let (ci, i') = unqualify i; (co, o') = unqualify o in (ci <> co, TStage i' o')
unqualify (TCollector i o) = let (ci, i') = unqualify i; (co, o') = unqualify o in (ci <> co, TCollector i' o')
unqualify (TStream eff i o r) =
  let (ci, i') = unqualify i; (co, o') = unqualify o; (cr, r') = unqualify r
  in (ci <> co <> cr, TStream eff i' o' r')
unqualify (TRecord row) = let (cs, bare) = rowQual row in (cs, TRecord bare)
 where
  rowQual (RExtend label ty rest) =
    let (cs, bare) = unqualify ty; (more, rest') = rowQual rest
    in (cs <> more, RExtend label bare rest')
  rowQual rest = ([], rest)
unqualify ty = ([], ty)

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

pattern TInt, TBool, TUnit, TString, TStatus, TBytes, TOverlay, TPatch :: Ty
pattern TInt = TCon "Int"
pattern TBool = TCon "Bool"
pattern TUnit = TCon "()"
pattern TString = TCon "String"
pattern TStatus = TCon "Status"
pattern TBytes = TCon "Bytes"
pattern TOverlay = TCon "Overlay"
pattern TPatch = TCon "Patch"

-- | Unknowns that stand for types, and unknowns that stand for rows.
data Subst = Subst
  { getSubst :: Map Var Ty
  , getRowSubst :: Map Var Row
  , getEffectSubst :: Map Var EffRow
  , effectConstraints :: [EffConstraint]
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
  | InconsistentEffects [EffConstraint]
  deriving (Eq, Show)

-- | The substitution that solves nothing.
emptySubst :: Subst
emptySubst = Subst Map.empty Map.empty Map.empty []

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
apply s (TJob eff t) = TJob (applyEffects s eff) (apply s t)
apply s (TStream eff i o r) = TStream (applyEffects s eff) (apply s i) (apply s o) (apply s r)
apply s (TQualified cs ty) = qualify (map (applyConstraint s) cs) (apply s ty)
apply s (TList t) = TList (apply s t)
apply s (TStage i o) = TStage (apply s i) (apply s o)
apply s (TCollector i o) = TCollector (apply s i) (apply s o)
apply _ t = t

-- | Replace each solved unknown in a row, in the tail and in the field types.
applyRow :: Subst -> Row -> Row
applyRow _ REmpty = REmpty
applyRow s (RVar v) = Map.findWithDefault (RVar v) v (getRowSubst s)
applyRow s (RExtend l t r) = RExtend l (apply s t) (applyRow s r)

applyEffects :: Subst -> EffRow -> EffRow
applyEffects sub (EffRow labels vars) = foldr unionEffects (EffRow labels Set.empty)
  [Map.findWithDefault (effectVar var) var (getEffectSubst sub) | var <- Set.toList vars]

applyConstraint :: Subst -> EffConstraint -> EffConstraint
applyConstraint sub (EffEqual a b) = EffEqual (applyEffects sub a) (applyEffects sub b)

-- | Compose like functions: @apply (compose s1 s2) = apply s1 . apply s2@.
-- So @s2@ happens first. All three maps need this: types can contain either
-- kind of row, and record fields can contain types with effect rows.
compose :: Subst -> Subst -> Subst
compose s1 s2 = Subst
  { getSubst = Map.map (apply s1) (getSubst s2) `Map.union` getSubst s1
  , getRowSubst = Map.map (applyRow s1) (getRowSubst s2) `Map.union` getRowSubst s1
  , getEffectSubst = Map.map (applyEffects s1) (getEffectSubst s2) `Map.union` getEffectSubst s1
  , effectConstraints = nub (map (applyConstraint s1) (effectConstraints s2) <> effectConstraints s1)
  }

-- | Does this unknown appear anywhere in this type?
occurs :: Var -> Ty -> Bool
occurs v (TVar v') = v == v'
occurs v (TFun t1 t2) = occurs v t1 || occurs v t2
occurs v (TRecord r) = occursRow v r
occurs v (TComp (EffRow _ vars) t) = Set.member v vars || occurs v t
occurs v (TJob (EffRow _ vars) t) = Set.member v vars || occurs v t
occurs v (TStream (EffRow _ vars) i o r) = Set.member v vars || any (occurs v) [i, o, r]
occurs v (TQualified cs ty) = any (Set.member v . constraintVars) cs || occurs v ty
occurs v (TList t) = occurs v t
occurs v (TStage i o) = occurs v i || occurs v o
occurs v (TCollector i o) = occurs v i || occurs v o
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

-- | Find a substitution and obligations that make two types equal.
--
-- Note: what you learn from one side of a 'TFun' has to apply to the other.
unify :: Ty -> Ty -> Unifier Subst
unify left right = do
  let (a, bareLeft) = unqualify left
      (b, bareRight) = unqualify right
  sub <- unifyBare bareLeft bareRight
  pure (sub `compose` emptySubst {effectConstraints = nub (a <> b)})

unifyBare :: Ty -> Ty -> Unifier Subst
unifyBare t1 t2 | t1 == t2 = pure emptySubst
unifyBare (TVar v) t = lift (bind v t)
unifyBare t (TVar v) = lift (bind v t)
unifyBare (TFun l1 r1) (TFun l2 r2) = do
  s1 <- unify l1 l2
  s2 <- unify (apply s1 r1) (apply s1 r2)
  pure (s2 `compose` s1)
unifyBare (TRecord r1) (TRecord r2) = unifyRow r1 r2
unifyBare (TList a) (TList b) = unify a b
unifyBare (TStage i o) (TStage i' o') = unify (TFun i o) (TFun i' o')
unifyBare (TCollector i o) (TCollector i' o') = unify (TFun i o) (TFun i' o')
unifyBare (TComp eff a) (TComp eff' b) = do
  s1 <- unify a b
  s2 <- unifyEffects (applyEffects s1 eff) (applyEffects s1 eff')
  pure (s2 `compose` s1)
unifyBare (TJob eff a) (TJob eff' b) = do
  s1 <- unify a b
  s2 <- unifyEffects (applyEffects s1 eff) (applyEffects s1 eff')
  pure (s2 `compose` s1)
unifyBare (TStream eff i o r) (TStream eff' i' o' r') = do
  s1 <- unify i i'
  s2 <- unify (apply s1 o) (apply s1 o')
  let known = s2 `compose` s1
  s3 <- unify (apply known r) (apply known r')
  let types = s3 `compose` known
  s4 <- unifyEffects (applyEffects types eff) (applyEffects types eff')
  pure (s4 `compose` types)
unifyBare t1 t2 = lift (Left (Mismatch t1 t2))

-- | Solve exact effect-row equality. Unions are idempotent. A bare unknown
-- can take any row; self-absorption introduces an open residual. More general
-- set equalities remain as qualifications until later substitutions resolve
-- them. They must stay satisfiable together, not merely one at a time.
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
  | otherwise = lift (solveConstraints emptySubst {effectConstraints = [EffEqual left right]})
 where
  bindEffect var row@(EffRow fixed tails)
    | Set.member var tails = do
        residual <- freshVar
        pure (singletonEffect var (EffRow fixed (Set.insert residual (Set.delete var tails))))
    | otherwise = pure (singletonEffect var row)

-- | Simplify deterministic equalities and keep the remaining qualifications.
-- No arbitrary split of a set union is chosen. Check the whole conjunction,
-- since individually satisfiable bounds can contradict one another.
solveConstraints :: Subst -> Either TypeError Subst
solveConstraints original = loop original (effectConstraints original)
 where
  loop sub equations =
    let pending = nub [EffEqual a b | equation <- equations,
          let EffEqual a b = applyConstraint sub equation, a /= b]
    in case firstBinding pending of
      Just (var, row) ->
        let next = singletonEffect var row `compose` sub {effectConstraints = []}
        in loop next pending
      Nothing
        | satisfiableEffects pending -> Right sub {effectConstraints = pending}
        | [EffEqual a@(EffRow _ av) b@(EffRow _ bv)] <- pending,
          Set.null av && Set.null bv -> Left (EffectMismatch a b)
        | otherwise -> Left (InconsistentEffects pending)
  firstBinding [] = Nothing
  firstBinding (EffEqual a@(EffRow fixed vars) b@(EffRow fixed' vars') : rest)
    | Set.null fixed, [var] <- Set.toList vars, not (Set.member var vars') = Just (var, b)
    | Set.null fixed', [var] <- Set.toList vars', not (Set.member var vars) = Just (var, a)
    | a == noEffects, Set.null fixed', var : _ <- Set.toList vars' = Just (var, noEffects)
    | b == noEffects, Set.null fixed, var : _ <- Set.toList vars = Just (var, noEffects)
    | otherwise = firstBinding rest

-- For each effect label, a row equality is a dual-Horn Boolean formula.
-- A clause has at most one negative literal. Start from the greatest model
-- (all variables true) and propagate forced false variables to a fixed point.
-- Nothing denotes any label not explicitly named, which must be checked too.
type Clause = (Maybe Var, Set Var)

clauses :: Maybe Effect -> [EffConstraint] -> [Clause]
clauses label = concatMap rowClauses
 where
  rowClauses (EffEqual (EffRow a av) (EffRow b bv)) =
    case (maybe False (`Set.member` a) label, maybe False (`Set.member` b) label) of
      (True, True) -> []
      (True, False) -> [(Nothing, bv)]
      (False, True) -> [(Nothing, av)]
      (False, False) -> [(Just v, bv) | v <- Set.toList av] <> [(Just v, av) | v <- Set.toList bv]

satisfiableClauses :: [Clause] -> Bool
satisfiableClauses formula = go Set.empty
 where
  go false =
    let forced = Set.fromList [v | (Just v, positive) <- formula, positive `Set.isSubsetOf` false]
        next = Set.union false forced
    in if next == false
      then all (\(negative, positive) -> negative /= Nothing || not (positive `Set.isSubsetOf` false)) formula
      else go next

constraintLabels :: [EffConstraint] -> Set Effect
constraintLabels = Set.unions . concatMap (\(EffEqual (EffRow a _) (EffRow b _)) -> [a, b])

satisfiableEffects :: [EffConstraint] -> Bool
satisfiableEffects equations = all (\label -> satisfiableClauses (clauses label equations))
  (Nothing : map Just (Set.toList (constraintLabels equations)))

-- | Prove that the outer action has one closed effect set. A residual row can
-- still be ambiguous internally while its union with fixed labels is exact.
closeEffects :: [EffConstraint] -> EffRow -> Maybe EffRow
closeEffects equations (EffRow fixed vars)
  | not (satisfiableEffects equations) = Nothing
  | possible Nothing = Nothing
  | otherwise = EffRow <$> foldLabels (Set.toList labels) <*> pure Set.empty
 where
  labels = Set.union fixed (constraintLabels equations)
  possible label = maybe False (`Set.member` fixed) label ||
    satisfiableClauses ((Nothing, vars) : clauses label equations)
  absent label = not (maybe False (`Set.member` fixed) label) &&
    satisfiableClauses ([(Just v, Set.empty) | v <- Set.toList vars] <> clauses label equations)
  foldLabels [] = Just Set.empty
  foldLabels (label : rest)
    | possible (Just label) && absent (Just label) = Nothing
    | possible (Just label) = Set.insert label <$> foldLabels rest
    | otherwise = foldLabels rest

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
