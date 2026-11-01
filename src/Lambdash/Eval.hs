-- | The evaluator: what an expression means, once the checker has passed it.
--
-- This is pure. A computation is a value that holds an action, and evaluating it
-- does not run it.
module Lambdash.Eval
  ( Value (..),
    Scope,
    render,
    applyValue,
    select,
    runComp,
    eval,
    builtins,
    typeEnv,
    scope,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Lambdash.Infer (Arg (..), Env, Expr (..), Lit (..), Name, Scheme (..), mono)
import Lambdash.Process (Status (..), runExternal)
import Lambdash.Unify

-- | What an expression can evaluate to. Fields stay in written order, because
-- that is the order to show them in. The checker says that a field exists, and
-- 'select' finds the first one with that label.
data Value
  = VInt Int
  | VBool Bool
  | VUnit
  | VStr String
  | VStatus Status
  | VRecord [(Label, Value)]
  | -- | The name identifies the builtin.
    VBuiltin Name (Value -> Value)
  | VClosure Name Expr Scope
  | -- | An action. Building one runs nothing: only 'runComp' does.
    VComp (IO Value)

-- | What each name in scope has as its value.
type Scope = Map Name Value

-- | Show a value the way the REPL will: @1@, @true@, @{a = 1, b = true}@. All
-- functions show as @<function>@.
render :: Value -> String
render (VInt n) = show n
render (VBool b) = if b then "true" else "false"
render VUnit = "()"
render (VStr s) = show s
render (VStatus (ExitedWith n)) = "exit " <> show n
render (VStatus (KilledBy n)) = "signal " <> show n
render (VRecord fields) = "{" <> commaSep (map renderField fields) <> "}"
  where
    renderField (label, value) = label <> " = " <> render value
render (VBuiltin _ _) = "<function>"
render (VClosure _ _ _) = "<function>"
render (VComp _) = "<computation>"

commaSep :: [String] -> String
commaSep = foldr step ""
  where
    step item "" = item
    step item rest = item <> ", " <> rest

-- | Apply a function value to an argument.
--
-- Note: only a type error gets to a value that is not a function, and the
-- checker has already refused those, so 'error' is fine in that case.
applyValue :: Value -> Value -> Value
applyValue (VBuiltin _ f) value = f value
applyValue (VClosure name body captured) value = eval (Map.insert name value captured) body
applyValue _ _ = error "applyValue: the checker passed a non-function"

-- | Read a field from a record value.
select :: Label -> Value -> Value
select label (VRecord fields) =
  case lookup label fields of
    Just value -> value
    Nothing -> error "select: the checker passed a missing field"
select _ _ = error "select: the checker passed a non-record"

-- | Run a computation value. This is the only place where effects happen.
runComp :: Value -> IO Value
runComp (VComp action) = action
runComp _ = error "runComp: the checker passed a non-computation"

-- | Evaluate an expression that passed the checker.
eval :: Scope -> Expr -> Value
eval env expression =
  case expression of
    EVar name ->
      case Map.lookup name env of
        Just value -> value
        Nothing -> error "eval: unbound name"
    ELit literal -> evalLit literal
    EApp function argument -> applyValue (eval env function) (eval env argument)
    ELam name body -> VClosure name body env
    ERecord fields -> VRecord [(label, eval env value) | (label, value) <- fields]
    ESelect record label -> select label (eval env record)
    ELet name value body -> eval (Map.insert name (eval env value) env) body
    ECmd name args -> VComp (VStatus <$> runExternal name (map word args))
  where
    evalLit (LInt n) = VInt n
    evalLit (LBool b) = VBool b
    evalLit LUnit = VUnit
    evalLit (LStr s) = VStr s
    word (AWord s) = s
    word (AVar name) = case Map.lookup name env of
      Just (VStr s) -> s
      _ -> error "eval: the checker passed a non-String argument"

-- | The names that exist before any input, with a type and a value together, so
-- that the two cannot drift apart.
builtins :: [(Name, Scheme, Value)]
builtins =
  [ ("not", mono (TFun TBool TBool), VBuiltin "not" notV),
    ("add", mono (TFun TInt (TFun TInt TInt)), VBuiltin "add" add),
    ("print", Forall [t] (TFun (TVar t) (TComp TUnit)), VBuiltin "print" printV),
    ("pure", Forall [t] (TFun (TVar t) (TComp (TVar t))), VBuiltin "pure" (VComp . pure))
  ]
  where
    t = Var 0
    printV (VStr s) = VComp (VUnit <$ putStrLn s)
    printV value = VComp (VUnit <$ putStrLn (render value))
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
