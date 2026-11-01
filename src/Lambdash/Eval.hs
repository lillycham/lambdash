-- | The evaluator: what an expression means, once the checker has passed it.
--
-- This is pure for now. Computations arrive with the value and computation split.
module Lambdash.Eval
  ( Value (..)
  , Scope
  , render
  , applyValue
  , select
  , eval
  , builtins
  , typeEnv
  , scope
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Lambdash.Infer (Env, Expr (..), Lit (..), Name, Scheme, mono)
import Lambdash.Unify

-- | What an expression can evaluate to. Fields stay in written order, because
-- that is the order to show them in. The checker says that a field exists, and
-- 'select' finds the first one with that label.
data Value
  = VInt Int
  | VBool Bool
  | VRecord [(Label, Value)]
  | -- | The name is for 'render'.
    VBuiltin Name (Value -> Value)
  | -- | Add what a function value must carry here, then remove this comment.
    VClosure

-- | What each name in scope has as its value.
type Scope = Map Name Value

-- | Show a value the way the REPL will: @1@, @true@, @{a = 1, b = true}@. All
-- functions show as @<function>@.
render :: Value -> String
render = undefined

-- | Apply a function value to an argument.
--
-- Note: only a type error gets to a value that is not a function, and the
-- checker has already refused those, so 'error' is fine in that case.
applyValue :: Value -> Value -> Value
applyValue = undefined

-- | Read a field from a record value.
select :: Label -> Value -> Value
select = undefined

-- | Evaluate an expression that passed the checker.
eval :: Scope -> Expr -> Value
eval = undefined

-- | The names that exist before any input, with a type and a value together, so
-- that the two cannot drift apart.
builtins :: [(Name, Scheme, Value)]
builtins =
  [ ("not", mono (TFun TBool TBool), VBuiltin "not" notV)
  , ("add", mono (TFun TInt (TFun TInt TInt)), VBuiltin "add" add)
  ]
 where
  notV (VBool b) = VBool (not b)
  notV _ = error "not: the checker passed a non-Bool"
  add (VInt a) = VBuiltin "add" (addTo a)
  add _ = error "add: the checker passed a non-Int"
  addTo a (VInt b) = VInt (a + b)
  addTo _ _ = error "add: the checker passed a non-Int"

-- | The builtins as the checker sees them.
typeEnv :: Env
typeEnv = Map.fromList [(n, t) | (n, t, _) <- builtins]

-- | The builtins as the evaluator sees them.
scope :: Scope
scope = Map.fromList [(n, x) | (n, _, x) <- builtins]
