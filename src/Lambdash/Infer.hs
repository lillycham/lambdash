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

import Control.Monad.Trans.State.Strict (StateT, evalStateT)
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
fresh = undefined

-- | Apply a substitution to every type in the environment.
applyEnv :: Subst -> Env -> Env
applyEnv = undefined

-- | Find the type of an expression, and what that taught us.
--
-- Note: the 'Subst' from each step has to reach the steps after it, in the
-- environment and in the types you already hold.
infer :: Env -> Expr -> Infer (Subst, Ty)
infer = undefined

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

