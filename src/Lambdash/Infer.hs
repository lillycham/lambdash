-- | Type inference for a small core: variables, lambdas, application.
--
-- This is Algorithm W without @let@. Each step returns what it learned as a
-- substitution, together with the type it found.
module Lambdash.Infer
  ( Name
  , Lit (..)
  , Expr (..)
  , Env
  , InferError (..)
  , Infer
  , fresh
  , applyEnv
  , infer
  , inferIn
  , inferType
  ) where

import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Lambdash.Unify

type Name = String

data Lit
  = LInt Int
  | LBool Bool
  deriving (Eq, Show)

data Expr
  = EVar Name
  | ELit Lit
  | ELam Name Expr
  | EApp Expr Expr
  deriving (Eq, Show)

-- | What each name in scope has as its type.
type Env = Map Name Ty

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

-- | Apply a substitution to every type in the environment.
applyEnv :: Subst -> Env -> Env
applyEnv s e = Map.map (apply s) e

-- | Find the type of an expression, and what that taught us.
--
-- Note: the 'Subst' from each step has to reach the steps after it, in the
-- environment and in the types you already hold.
-- | Find the type of an expression, and what that taught us.
infer :: Env -> Expr -> Infer (Subst, Ty)
infer env (EVar name) =
  case Map.lookup name env of
    Nothing -> lift (Left (Unbound name))
    Just t  -> pure (emptySubst, t)

infer _ (ELit (LInt _))  = pure (emptySubst, TInt)
infer _ (ELit (LBool _)) = pure (emptySubst, TBool)

infer env (ELam name body) = do
  tv <- fresh
  let env' = Map.insert name tv env
  (s, t) <- infer env' body
  pure (s, apply s (TFun tv t))

infer env (EApp e1 e2) = do
  (s1, t1) <- infer env e1
  (s2, t2) <- infer (applyEnv s1 env) e2
  tv <- fresh
  let t1' = apply s2 t1
  s3 <- case unify t1' (TFun t2 tv) of
    Left err -> lift (Left (Unify err))
    Right s  -> pure s
  let s_final = s3 `compose` s2 `compose` s1
  pure (s_final, apply s3 tv)

-- | Infer in a starting environment, with the final solution applied.
inferIn :: Env -> Expr -> Either InferError Ty
inferIn env expr = evalStateT (go env expr) 0
 where
  go e x = do
    (s, t) <- infer e x
    pure (apply s t)

-- | Infer with nothing in scope.
inferType :: Expr -> Either InferError Ty
inferType = inferIn Map.empty

