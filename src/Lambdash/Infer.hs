-- | Type inference for a small core: variables, lambdas, application, @let@.
--
-- This is Algorithm W. Each step returns what it learned as a
-- substitution, together with the type it found.
module Lambdash.Infer
  ( Name
  , Lit (..)
  , Arg (..)
  , Expr (..)
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
  | -- | @r.name@.
    ESelect Expr Label
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
ftv (TComp t) = ftv t
ftv (TStream i o r) = Set.unions (map ftv [i, o, r])

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
    }

-- | Apply a substitution to every scheme in the environment.
applyEnv :: Subst -> Env -> Env
applyEnv s e = Map.map (applyScheme s) e

-- | Bind each unknown of the type that the environment does not mention, in
-- ascending order.
--
-- Note: the caller has to apply what it learned to the environment first.
generalise :: Env -> Ty -> Scheme
generalise env t = Forall as t
  where as = Set.toList $ ftv t `Set.difference` ftvEnv env

-- | Replace each bound unknown with a fresh one, so that each use of a name is
-- its own copy.
instantiate :: Scheme -> Infer Ty
instantiate (Forall bound ty) = do
  freshVars <- mapM (const (unifying freshVar)) bound
  let renaming = Map.fromList (zip bound freshVars)
      sub = Subst
        { getSubst = TVar <$> renaming
        , getRowSubst = RVar <$> renaming
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
      pure (emptySubst, t)

infer _ (ELit (LInt _))  = pure (emptySubst, TInt)
infer _ (ELit (LBool _)) = pure (emptySubst, TBool)
infer _ (ELit LUnit)     = pure (emptySubst, TUnit)
infer _ (ELit (LStr _))  = pure (emptySubst, TString)

-- For now, an argument must be a String. The checker has nothing to convert with.
infer env (ECmd _ args) = do
  subs <- mapM check args
  pure (foldr compose emptySubst subs, TStream TBytes TBytes TStatus)
 where
  check (AWord _) = pure emptySubst
  check (AVar name) = do
    (s, t) <- infer env (EVar name)
    unifying (unify (apply s t) TString)

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
      unifying (unify (apply s2 leftTy) (TStream input output result))
    _ -> pure emptySubst
  let known = shape `compose` s2 `compose` s1
      left' = apply shape (apply s2 leftTy)
      right' = apply shape rightTy
  (s3, result) <- case (left', right') of
    (TStream input output _, TStream input' output' result') -> do
      s <- unifying (unify output input')
      pure (s, TStream input output' result')
    (TStream input output result', _) -> do
      item <- fresh
      s <- unifying (unify right' (TFun output item))
      pure (s, TStream input item result')
    (TComp value, TStream input output result') -> do
      s <- unifying (unify value input)
      pure (s, TStream TBytes output result')
    (_, TStream input output result') -> do
      s <- unifying (unify left' input)
      pure (s, TStream TBytes output result')
    (TComp value, _) -> do
      item <- fresh
      s <- unifying (unify right' (TFun value item))
      pure (s, TComp item)
    _ -> do
      item <- fresh
      s <- unifying (unify right' (TFun left' item))
      pure (s, item)
  pure (s3 `compose` known, apply s3 result)

infer env (ELet x e1 e2) = do
  (s1, t1) <- infer env e1
  let env' = applyEnv s1 env
      sc = generalise env' (apply s1 t1)
  (s2, t2) <- infer (Map.insert x sc env') e2
  pure (s2 `compose` s1, apply s2 t2)

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

-- | The external commands that an expression names, also under lambdas.
commands :: Expr -> [Name]
commands (ECmd name _) = [name]
commands (ELam _ body) = commands body
commands (EApp f x) = commands f <> commands x
commands (EPipe left right) = commands left <> commands right
commands (ELet _ bound body) = commands bound <> commands body
commands (ERecord fields) = concatMap (commands . snd) fields
commands (ESelect record _) = commands record
commands _ = []

-- | Infer in a starting environment, with the final solution applied.
inferIn :: Env -> Expr -> Either InferError Ty
inferIn env expr = evalStateT (go env expr) 0
 where
  go e x = do
    (s, t) <- infer e x
    pure (apply s t)

-- | The return type of a computation or stream. A stream's output items are
-- separate from its return value. An unknown action defaults to @TComp a@.
inferRunIn :: Env -> Expr -> Either InferError Ty
inferRunIn env expr = evalStateT go 0
 where
  go = do
    (s1, t) <- infer env expr
    case apply s1 t of
      TStream _ _ result -> pure result
      ty -> do
        result <- fresh
        s2 <- unifying (unify ty (TComp result))
        pure (apply s2 result)

-- | Infer with nothing in scope.
inferType :: Expr -> Either InferError Ty
inferType = inferIn Map.empty

