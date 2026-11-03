-- | Type inference for a small core: variables, lambdas, application, @let@.
--
-- This is Algorithm W. Each step returns what it learned as a
-- substitution, together with the type it found.
module Lambdash.Infer
  ( Name
  , Lit (..)
  , Arg (..)
  , Expr (..)
  , Stmt (..)
  , commands
  , Scheme (..)
  , mono
  , Env
  , InferError (..)
  , Infer
  , fresh
  , unifying
  , ftv
  , ftvRow
  , ftvEffects
  , actionEffects
  , ftvScheme
  , ftvEnv
  , applyScheme
  , applyEnv
  , generalise
  , instantiate
  , infer
  , inferIn
  , inferRunIn
  , inferType
  ) where

import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, mapStateT, put)
import Control.Monad.Trans.Class (lift)
import Control.Monad (foldM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Lambdash.Unify
import qualified Data.Set as Set

type Name = String

data Lit
  = LInt Int
  | LBool Bool
  | LUnit
  | LStr String
  | -- | A bare flake reference, such as @nixpkgs#ffmpeg@. It is a String.
    LRef String
  deriving (Eq, Show)

-- | One argument of an external command.
data Arg
  = -- | A bareword or a string literal.
    AWord String
  | -- | @$x@.
    AVar Name
  deriving (Eq, Show)

data Expr
  = EVar Name
  | ELit Lit
  | ELam Name Expr
  | EApp Expr Expr
  | EPipe Expr Expr
  | -- | @let name = bound in body@. It is not recursive: @name@ is not in scope
    -- in @bound@.
    ELet Name Expr Expr
  | -- | @git status@: a head that nothing binds, and its arguments.
    ECmd Name [Arg]
  | -- | @{name = 1, ok = true}@.
    ERecord [(Label, Expr)]
  | EList [Expr]
  | -- | @r.name@.
    ESelect Expr Label
  | -- | @do s; s; e end@: run each statement in order. The value is the last
    -- statement's result.
    EDo [Stmt]
  | -- | @with overlay do ... end@: run the block with one more environment
    -- layer, for its dynamic extent only.
    EWith Expr Expr
  | -- | @e &@: start @e@ as a background job, and give its handle.
    EBg Expr
  deriving (Eq, Show)

-- | One statement of a @do@ block.
data Stmt
  = -- | @x <- e@: run @e@ and name its result for the statements after it.
    SBind Name Expr
  | -- | @x = e@: name a value. Nothing runs.
    SLet Name Expr
  | -- | @e@: run it. Its result is dropped, unless it is the last statement.
    SRun Expr
  deriving (Eq, Show)

-- | A type that holds for every choice of the listed unknowns.
-- @Forall [a] (a -> a)@ is @forall a. a -> a@. The other unknowns are free.
data Scheme = Forall [Var] Ty
  deriving (Eq, Show)

-- | A type with nothing to choose. Lambda parameters have this.
mono :: Ty -> Scheme
mono = Forall []

-- | What each name in scope has as its type.
type Env = Map Name Scheme

data InferError
  = -- | Two types did not unify.
    Unify TypeError
  | -- | A name that nothing binds.
    Unbound Name
  deriving (Eq, Show)

-- | The state is the next unused unknown. Failure is 'InferError'. Use @lift@
-- from "Control.Monad.Trans.Class" to run an @Either InferError a@ in here.
type Infer = StateT Int (Either InferError)

-- | Make an unknown that nothing has used yet.
fresh :: Infer Ty
fresh = do
  n <- get
  put (n + 1)
  pure (TVar (Var n))

-- | Run unification in here.
unifying :: Unifier a -> Infer a
unifying = mapStateT (either (Left . Unify) Right)

-- | The unknowns that appear in a type.
--
-- Note: this has to see row unknowns as well, or @generalise@ misses them.
ftv :: Ty -> Set Var
ftv (TVar v) = Set.singleton v
ftv (TFun t1 t2) = ftv t1 `Set.union` ftv t2
ftv (TCon _) = Set.empty
ftv (TRecord r) = ftvRow r
ftv (TComp eff t) = ftvEffects eff `Set.union` ftv t
ftv (TJob eff t) = ftvEffects eff `Set.union` ftv t
ftv (TStream eff i o r) = Set.unions (ftvEffects eff : map ftv [i, o, r])
ftv (TQualified cs ty) = Set.unions (ftv ty : map constraintVars cs)
ftv (TList ty) = ftv ty
ftv (TStage i o) = ftv i `Set.union` ftv o
ftv (TCollector i o) = ftv i `Set.union` ftv o

ftvEffects :: EffRow -> Set Var
ftvEffects (EffRow _ vars) = vars

-- | Only the outer action executes. A returned action, or one inside a
-- record, stays latent until it is run separately.
actionEffects :: Ty -> EffRow
actionEffects (TComp eff _) = eff
actionEffects (TStream eff _ _ _) = eff
actionEffects (TQualified _ ty) = actionEffects ty
actionEffects _ = noEffects

-- | The unknowns that appear in a row.
ftvRow :: Row -> Set Var
ftvRow REmpty = Set.empty
ftvRow (RVar v) = Set.singleton v
ftvRow (RExtend _ t r) = ftv t `Set.union` ftvRow r

-- | The unknowns that a scheme does not bind.
ftvScheme :: Scheme -> Set Var
ftvScheme (Forall bound ty) = ftv ty `Set.difference` Set.fromList bound

-- | The unknowns free in any scheme in the environment. These are the unknowns
-- that something outside a @let@ can still constrain.
ftvEnv :: Env -> Set Var
ftvEnv = Set.unions . map ftvScheme . Map.elems

-- | Apply a substitution to the free unknowns of a scheme. A bound unknown only
-- marks a place to fill in, so the substitution must not touch it.
applyScheme :: Subst -> Scheme -> Scheme
applyScheme sub (Forall bound ty) =
  Forall bound (apply freeSubst ty)
 where
  boundVars = Set.fromList bound
  freeSubst = Subst
    { getSubst = Map.withoutKeys (getSubst sub) boundVars
    , getRowSubst = Map.withoutKeys (getRowSubst sub) boundVars
    , getEffectSubst = Map.withoutKeys (getEffectSubst sub) boundVars
    , effectConstraints = []
    }

-- | Apply a substitution to every scheme in the environment.
applyEnv :: Subst -> Env -> Env
applyEnv s e = Map.map (applyScheme s) e

-- | Bind each unknown of the type that the environment does not mention, in
-- ascending order.
--
-- Note: the caller has to apply what it learned to the environment first.
generalise :: Env -> Ty -> Scheme
generalise env t = Forall as qualified
 where
  (constraints, bare) = unqualify t
  relevant = connectedConstraints (ftv bare `Set.union` ftvEnv env) constraints
  fixed = constraintClosure (ftvEnv env) relevant
  qualified = qualify relevant bare
  as = Set.toList (ftv qualified `Set.difference` fixed)

constraintClosure :: Set Var -> [EffConstraint] -> Set Var
constraintClosure seed constraints =
  let next = Set.unions (seed : [constraintVars c | c <- constraints,
        not (Set.null (Set.intersection seed (constraintVars c)))])
  in if next == seed then seed else constraintClosure next constraints

connectedConstraints :: Set Var -> [EffConstraint] -> [EffConstraint]
connectedConstraints seed constraints =
  let connected = constraintClosure seed constraints
  in [c | c <- constraints, not (Set.null (Set.intersection connected (constraintVars c)))]

-- | Replace each bound unknown with a fresh one, so that each use of a name is
-- its own copy.
instantiate :: Scheme -> Infer Ty
instantiate (Forall bound ty) = do
  freshVars <- mapM (const (unifying freshVar)) bound
  let renaming = Map.fromList (zip bound freshVars)
      sub = Subst
        { getSubst = TVar <$> renaming
        , getRowSubst = RVar <$> renaming
        , getEffectSubst = effectVar <$> renaming
        , effectConstraints = []
        }
  pure (apply sub ty)

-- | Find the type of an expression, and what that taught us.
--
-- Note: the 'Subst' from each step has to reach the steps after it, in the
-- environment and in the types you already hold.
-- | Find the type of an expression, and what that taught us.
infer :: Env -> Expr -> Infer (Subst, Ty)
infer env (EVar name) =
  case Map.lookup name env of
    Nothing -> lift (Left (Unbound name))
    Just sc -> do
      t <- instantiate sc
      let (constraints, bare) = unqualify t
      pure (emptySubst {effectConstraints = constraints}, bare)

infer _ (ELit (LInt _))  = pure (emptySubst, TInt)
infer _ (ELit (LBool _)) = pure (emptySubst, TBool)
infer _ (ELit LUnit)     = pure (emptySubst, TUnit)
infer _ (ELit (LStr _))  = pure (emptySubst, TString)
infer _ (ELit (LRef _))  = pure (emptySubst, TString)

-- For now, an argument must be a String. The checker has nothing to convert with.
infer env (ECmd command args) = do
  sub <- foldM check emptySubst args
  pure (sub, TStream (effect (Exec command)) TBytes TBytes TStatus)
 where
  check known (AWord _) = pure known
  check known (AVar name) = do
    (s, t) <- infer (applyEnv known env) (EVar name)
    checked <- unifying (unify (apply s t) TString)
    pure (checked `compose` s `compose` known)

infer env (ELam name body) = do
  tv <- fresh
  let env' = Map.insert name (mono tv) env
  (s, t) <- infer env' body
  pure (s, apply s (TFun tv t))

infer env (EApp e1 e2) = do
  (s1, t1) <- infer env e1
  (s2, t2) <- infer (applyEnv s1 env) e2
  tv <- fresh
  let t1' = apply s2 t1
  s3 <- unifying (unify t1' (TFun t2 tv))
  let s_final = s3 `compose` s2 `compose` s1
  pure (s_final, apply s3 tv)

infer env (EPipe left right) = do
  (s1, leftTy) <- infer env left
  (s2, rightTy) <- infer (applyEnv s1 env) right
  -- An unknown producer is a stream. Deferring this choice to evaluation
  -- would let a polymorphic function change meaning after instantiation.
  shape <- case apply s2 leftTy of
    TVar _ -> do
      input <- fresh
      output <- fresh
      result <- fresh
      eff <- unifying (effectVar <$> freshVar)
      unifying (unify (apply s2 leftTy) (TStream eff input output result))
    _ -> pure emptySubst
  let known = shape `compose` s2 `compose` s1
      left' = apply shape (apply s2 leftTy)
      right' = apply shape rightTy
  (s3, result) <- case (left', right') of
    (TStream eff input output result', TCollector item collected) -> do
      a <- unifying (unify output item)
      b <- unifying (unify (apply a input) TBytes)
      pure (b `compose` a, TComp eff (collectedTy collected result'))
    (TStage input output, TCollector item collected) -> do
      s <- unifying (unify output item)
      pure (s, TCollector input collected)
    (TComp eff value, TCollector item collected) -> do
      s <- unifying (unify value item)
      pure (s, TComp eff (collectedTy collected TUnit))
    (_, TCollector item collected) -> do
      s <- unifying (unify left' item)
      pure (s, TComp noEffects (collectedTy collected TUnit))
    (TStream eff input output result', TStage item output') -> do
      s <- unifying (unify output item)
      pure (s, TStream eff input output' result')
    (TStage input output, TStage item output') -> do
      s <- unifying (unify output item)
      pure (s, TStage input output')
    (TComp eff value, TStage input output) -> do
      s <- unifying (unify value input)
      pure (s, TStream eff TBytes output TUnit)
    (_, TStage input output) -> do
      s <- unifying (unify left' input)
      pure (s, TStream noEffects TBytes output TUnit)
    (TStage input output, _) -> do
      item <- fresh
      s <- unifying (unify right' (TFun output item))
      pure (s, TStage input item)
    (TStream eff input output _, TStream eff' input' output' result') -> do
      s <- unifying (unify output input')
      pure (s, TStream (unionEffects eff eff') input output' result')
    (TStream eff input output result', _) -> do
      item <- fresh
      s <- unifying (unify right' (TFun output item))
      pure (s, TStream eff input item result')
    (TComp eff value, TStream eff' input output result') -> do
      s <- unifying (unify value input)
      pure (s, TStream (unionEffects eff eff') TBytes output result')
    (_, TStream eff input output result') -> do
      s <- unifying (unify left' input)
      pure (s, TStream eff TBytes output result')
    (TComp eff value, _) -> do
      item <- fresh
      s <- unifying (unify right' (TFun value item))
      pure (s, TComp eff item)
    _ -> do
      item <- fresh
      s <- unifying (unify right' (TFun left' item))
      pure (s, item)
  pure (s3 `compose` known, apply s3 result)
 where
  collectedTy item result = TRecord (RExtend "items" (TList item) (RExtend "status" result (RExtend "statuses" (TList TStatus) REmpty)))

infer env (EList items) = do
  itemTy <- fresh
  sub <- foldM (check itemTy) emptySubst items
  pure (sub, TList (apply sub itemTy))
 where
  check itemTy known item = do
    (learned, ty) <- infer (applyEnv known env) item
    let combined = learned `compose` known
    equal <- unifying (unify (apply combined itemTy) ty)
    pure (equal `compose` combined)

infer env (ELet x e1 e2) = do
  (raw, t1) <- infer env e1
  s1 <- checkedConstraints raw
  let env' = applyEnv s1 env
      sc = generalise env' (qualify (effectConstraints s1) (apply s1 t1))
      -- Constraints involving an enclosing monomorphic variable must escape
      -- even when the local binding is unused. Independent quantified rows
      -- belong to the scheme and must not leak between its instances.
      escaped = connectedConstraints (ftvEnv env') (effectConstraints s1)
  (s2, t2) <- infer (Map.insert x sc env') e2
  pure (s2 `compose` s1 {effectConstraints = escaped}, apply s2 t2)

infer env (ERecord fields) = do
  (sub, row) <- inferFields env fields
  pure (sub, TRecord row)
 where
  inferFields :: Env -> [(Label, Expr)] -> Infer (Subst, Row)
  inferFields _ [] = pure (emptySubst, REmpty)
  inferFields fieldEnv ((label, expr) : rest) = do
    (fieldSub, fieldTy) <- infer fieldEnv expr
    (restSub, restRow) <- inferFields (applyEnv fieldSub fieldEnv) rest
    pure (restSub `compose` fieldSub, RExtend label (apply restSub fieldTy) restRow)

infer env (ESelect expr label) = do
  (exprSub, recordTy) <- infer env expr
  fieldTy <- fresh
  tailRow <- unifying (RVar <$> freshVar)
  let requiredTy = TRecord (RExtend label fieldTy tailRow)
  fieldSub <- unifying (unify recordTy requiredTy)
  pure (fieldSub `compose` exprSub, apply fieldSub fieldTy)

infer env (EDo stmts) = do
  (sub, effects, result) <- inferStmts env stmts
  let row = foldr (unionEffects . applyEffects sub) noEffects effects
  pure (sub, TComp row (apply sub result))

-- The checker drops the exec labels it can see in the body: the body runs
-- with the overlay, so the session's PATH says nothing about them. Each
-- pipeline still resolves its programs before it spawns anything. A label
-- hidden in an unknown tail, such as a parameter's, stays.
infer env (EWith overlay body) = do
  (s1, overlayTy) <- infer env overlay
  s2 <- unifying (unify overlayTy TOverlay)
  let known = s2 `compose` s1
  (s3, bodyTy) <- infer (applyEnv known env) body
  let sub = s3 `compose` known
  case apply sub bodyTy of
    TComp (EffRow labels tails) result ->
      pure (sub, TComp (EffRow (Set.filter (not . isExec) labels) tails) result)
    other -> do
      eff <- unifying (effectVar <$> freshVar)
      result <- fresh
      s4 <- unifying (unify other (TComp eff result))
      pure (s4 `compose` sub, apply s4 result)
 where
  isExec (Exec _) = True
  isExec _ = False

-- Starting a job starts its commands, so its row is the job's row, and the
-- preflight checks them before anything runs. The row also travels in the
-- handle, for wait and fg.
infer env (EBg e) = do
  (s, eff, result) <- inferRun env e
  pure (s, TComp eff (TJob eff result))

-- | The statements of a block: what they taught, each one's effect row, and
-- the last one's result. A run statement is a computation or a stream. A
-- stream reads the terminal, so its input is Bytes, and its result is its
-- return value, as at the prompt.
inferStmts :: Env -> [Stmt] -> Infer (Subst, [EffRow], Ty)
inferStmts _ [] = pure (emptySubst, [], TUnit)
inferStmts env [SRun e] = do
  (s, eff, result) <- inferRun env e
  pure (s, [eff], result)
inferStmts env (stmt : rest) = case stmt of
  SRun e -> do
    (s1, eff, _) <- inferRun env e
    (s2, effs, result) <- inferStmts (applyEnv s1 env) rest
    pure (s2 `compose` s1, eff : effs, result)
  SBind name e -> do
    (s1, eff, value) <- inferRun env e
    -- Like a lambda parameter, a bound result has one type in the block.
    let env' = Map.insert name (mono value) (applyEnv s1 env)
    (s2, effs, result) <- inferStmts env' rest
    pure (s2 `compose` s1, eff : effs, result)
  SLet name e -> do
    (raw, t1) <- infer env e
    s1 <- checkedConstraints raw
    let env' = applyEnv s1 env
        sc = generalise env' (qualify (effectConstraints s1) (apply s1 t1))
        escaped = connectedConstraints (ftvEnv env') (effectConstraints s1)
    (s2, effs, result) <- inferStmts (Map.insert name sc env') rest
    pure (s2 `compose` s1 {effectConstraints = escaped}, effs, result)

inferRun :: Env -> Expr -> Infer (Subst, EffRow, Ty)
inferRun env e = do
  (s1, t) <- infer env e
  case apply s1 t of
    TStream eff input _ result -> do
      s2 <- unifying (unify input TBytes)
      pure (s2 `compose` s1, applyEffects s2 eff, apply s2 result)
    TComp eff result -> pure (s1, eff, result)
    other -> do
      eff <- unifying (effectVar <$> freshVar)
      result <- fresh
      s2 <- unifying (unify other (TComp eff result))
      pure (s2 `compose` s1, applyEffects s2 eff, apply s2 result)

-- | The external commands that an expression names, also under lambdas.
commands :: Expr -> [Name]
commands (ECmd name _) = [name]
commands (ELam _ body) = commands body
commands (EApp f x) = commands f <> commands x
commands (EPipe left right) = commands left <> commands right
commands (ELet _ bound body) = commands bound <> commands body
commands (ERecord fields) = concatMap (commands . snd) fields
commands (EList items) = concatMap commands items
commands (ESelect record _) = commands record
commands (EDo stmts) = concatMap stmtCommands stmts
 where
  stmtCommands (SBind _ e) = commands e
  stmtCommands (SLet _ e) = commands e
  stmtCommands (SRun e) = commands e
commands (EWith overlay body) = commands overlay <> commands body
commands (EBg e) = commands e
commands _ = []

-- | Infer in a starting environment, with the final solution applied.
inferIn :: Env -> Expr -> Either InferError Ty
inferIn env expr = evalStateT (go env expr) (nextUnknown env)
 where
  go e x = do
    (s, t) <- infer e x
    finish e s t

checkedConstraints :: Subst -> Infer Subst
checkedConstraints = lift . either (Left . Unify) Right . solveConstraints

finish :: Env -> Subst -> Ty -> Infer Ty
finish env raw ty = do
  let (nested, bare) = unqualify ty
  solved <- checkedConstraints raw {effectConstraints = effectConstraints raw <> nested}
  let result = apply solved bare
      fixed = ftvEnv (applyEnv solved env)
      relevant = connectedConstraints (ftv result `Set.union` fixed) (effectConstraints solved)
  pure (qualify relevant result)

-- | The return type of a computation or stream. A stream's output items are
-- separate from its return value. An unknown action defaults to @TComp a@.
inferRunIn :: Env -> Expr -> Either InferError Ty
inferRunIn env expr = evalStateT go (nextUnknown env)
 where
  go = do
    (s1, t) <- infer env expr
    case apply s1 t of
      TStream _ input _ result -> do
        s2 <- unifying (unify input TBytes)
        finish env (s2 `compose` s1) result
      TComp _ result -> finish env s1 result
      ty -> do
        result <- fresh
        eff <- case ty of
          TVar _ -> unifying (effectVar <$> freshVar)
          _ -> pure noEffects
        s2 <- unifying (unify ty (TComp eff result))
        finish env (s2 `compose` s1) result

-- Include bound names too: a fresh substitution range must not capture a
-- quantified variable when that substitution reaches another scheme.
nextUnknown :: Env -> Int
nextUnknown env = 1 + maximum (-1 : [number | Forall bound ty <- Map.elems env,
  Var number <- Set.toList (Set.union (Set.fromList bound) (ftv ty))])

-- | Infer with nothing in scope.
inferType :: Expr -> Either InferError Ty
inferType = inferIn Map.empty

