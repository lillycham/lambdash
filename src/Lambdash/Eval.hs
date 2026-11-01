-- | The evaluator: what an expression means, once the checker has passed it.
--
-- This is pure. A computation is a value that holds an action, and evaluating it
-- does not run it.
module Lambdash.Eval
  ( Value (..),
    Return (..),
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
import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.List (isInfixOf, isPrefixOf)
import Data.IORef
import Control.Monad (when)
import Lambdash.Infer (Arg (..), Env, Expr (..), Lit (..), Name, Scheme (..), mono)
import Lambdash.Process (Status (..))
import Lambdash.Stream (Stage (..), runStages)
import Lambdash.Lines (runLines)
import Lambdash.Unify
import System.IO (stdout)

-- | What an expression can evaluate to. Fields stay in written order, because
-- that is the order to show them in. The checker says that a field exists, and
-- 'select' finds the first one with that label.
data Value
  = VInt Int
  | VBool Bool
  | VUnit
  | VStr String
  | VStatus Status
  | VBytes BS.ByteString
  | VRecord [(Label, Value)]
  | VList [Value]
  | -- | The name identifies the builtin.
    VBuiltin Name (Value -> Value)
  | VClosure Name Expr Scope
  | -- | An action. Building one runs nothing: only 'runComp' does.
    VComp (IO Value)
  | VStream [Stage Value] Return
  | VStage [Stage Value]
  | VCollector [Stage Value]

data Return = ReturnUnit | ReturnStatus

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
render (VBytes value) = "<" <> show (BS.length value) <> " bytes>"
render (VRecord fields) = "{" <> commaSep (map renderField fields) <> "}"
  where
    renderField (label, value) = label <> " = " <> render value
render (VList values) = "[" <> commaSep (map render values) <> "]"
render (VBuiltin _ _) = "<function>"
render (VClosure _ _ _) = "<function>"
render (VComp _) = "<computation>"
render (VStream _ _) = "<stream>"
render (VStage _) = "<stage>"
render (VCollector _) = "<collector>"

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

-- | Run a computation or stream. Output goes to stdout. Commands return a
-- status; pure sources return unit. Adapters preserve the source return.
runComp :: Value -> IO Value
runComp (VComp action) = action
runComp (VStream stages result) = runPlan result stages output
 where
  output (VBytes value) = BS.hPut stdout value
  output (VStr value) = putStrLn value
  output value = putStrLn (render value)
runComp _ = error "runComp: the checker passed a non-computation"

bytes :: Value -> BS.ByteString
bytes (VBytes value) = value
bytes _ = error "stream: the checker passed non-Bytes to an external"

runPlan :: Return -> [Stage Value] -> (Value -> IO ()) -> IO Value
runPlan result stages output = do
  status <- runStages VBytes bytes output stages
  pure $ case result of
    ReturnStatus -> VStatus status
    ReturnUnit -> VUnit

collectPlan :: Return -> [Stage Value] -> Value
collectPlan result stages = VComp $ do
  values <- newIORef []
  -- The final bridge routes even a raw external's stdout through the sink.
  status <- runPlan result (stages <> [MapStage id]) (\value -> modifyIORef' values (value :))
  items <- reverse <$> readIORef values
  pure (VRecord [("items", VList items), ("status", status)])

pipeValue :: Value -> Value -> Value
pipeValue (VStream left result) (VCollector right) = collectPlan result (left <> right)
pipeValue (VStage left) (VCollector right) = VCollector (left <> right)
pipeValue (VComp action) (VCollector right) = collectPlan ReturnUnit (SourceStage action : right)
pipeValue value (VCollector right) = collectPlan ReturnUnit (SourceStage (pure value) : right)
pipeValue (VStream left result) (VStage right) = VStream (left <> right) result
pipeValue (VStage left) (VStage right) = VStage (left <> right)
pipeValue (VComp action) (VStage right) = VStream (SourceStage action : right) ReturnUnit
pipeValue value (VStage right) = VStream (SourceStage (pure value) : right) ReturnUnit
pipeValue (VStage left) function = VStage (left <> [MapStage (applyValue function)])
pipeValue (VStream left _) (VStream right result) = VStream (left <> right) result
pipeValue (VStream left result) function = VStream (left <> [MapStage (applyValue function)]) result
pipeValue (VComp action) (VStream right result) = VStream (SourceStage action : right) result
pipeValue value (VStream right result) = VStream (SourceStage (pure value) : right) result
pipeValue (VComp action) function = VComp (applyValue function <$> action)
pipeValue value function = applyValue function value

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
    EPipe left right -> pipeValue (eval env left) (eval env right)
    ELam name body -> VClosure name body env
    ERecord fields -> VRecord [(label, eval env value) | (label, value) <- fields]
    EList items -> VList (map (eval env) items)
    ESelect record label -> select label (eval env record)
    ELet name value body -> eval (Map.insert name (eval env value) env) body
    ECmd name args -> VStream [External name (map word args)] ReturnStatus
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
    ("print", Forall [t] (TFun (TVar t) (TComp (effect Console) TUnit)), VBuiltin "print" printV),
    ("pure", Forall [t] (TFun (TVar t) (TComp noEffects (TVar t))), VBuiltin "pure" (VComp . pure)),
    ("encodeUtf8", mono (TFun TString TBytes), VBuiltin "encodeUtf8" encode),
    ("byteLength", mono (TFun TBytes TInt), VBuiltin "byteLength" byteLength),
    ("lines", mono (TStage TBytes TString), VStage [TransformStage lineStage]),
    ("filter", Forall [t] (TFun (TFun (TVar t) TBool) (TStage (TVar t) (TVar t))), VBuiltin "filter" filterV),
    ("map", Forall [t, u] (TFun (TFun (TVar t) (TVar u)) (TStage (TVar t) (TVar u))), VBuiltin "map" mapV),
    ("collect", Forall [t] (TCollector (TVar t) (TVar t)), VCollector []),
    ("each", Forall [t] (TFun (TList (TVar t)) (TStream noEffects TBytes (TVar t) TUnit)), VBuiltin "each" eachV),
    ("length", Forall [t] (TFun (TList (TVar t)) TInt), VBuiltin "length" lengthV),
    ("contains", mono (TFun TString (TFun TString TBool)), VBuiltin "contains" (stringPredicate isInfixOf)),
    ("startsWith", mono (TFun TString (TFun TString TBool)), VBuiltin "startsWith" (stringPredicate isPrefixOf)),
    ("isEmpty", mono (TFun TString TBool), VBuiltin "isEmpty" isEmptyV),
    ("stringLength", mono (TFun TString TInt), VBuiltin "stringLength" stringLength),
    ("encodeLine", mono (TFun TString TBytes), VBuiltin "encodeLine" encodeLine)
  ]
  where
    t = Var 0
    u = Var 1
    lineStage receive emit = runLines (fmap (fmap bytes) receive) (emit . VStr)
    mapV function = VStage [MapStage (applyValue function)]
    filterV predicate = VStage [TransformStage (\receive emit ->
      let loop = receive >>= maybe (pure ()) (\value -> do
            case applyValue predicate value of
              VBool keep -> when keep (emit value)
              _ -> error "filter: the checker passed a non-Bool predicate"
            loop)
      in loop)]
    eachV (VList values) = VStream [SourceListStage values] ReturnUnit
    eachV _ = error "each: the checker passed a non-list"
    lengthV (VList values) = VInt (length values)
    lengthV _ = error "length: the checker passed a non-list"
    stringPredicate predicate (VStr needle) = VBuiltin "string predicate" (\value -> case value of
      VStr text -> VBool (predicate needle text)
      _ -> error "string predicate: the checker passed a non-String")
    stringPredicate _ _ = error "string predicate: the checker passed a non-String"
    isEmptyV (VStr text) = VBool (null text)
    isEmptyV _ = error "isEmpty: the checker passed a non-String"
    stringLength (VStr text) = VInt (length text)
    stringLength _ = error "stringLength: the checker passed a non-String"
    encodeLine (VStr text) = VBytes (Text.encodeUtf8 (Text.pack (text <> "\n")))
    encodeLine _ = error "encodeLine: the checker passed a non-String"
    encode (VStr value) = VBytes (Text.encodeUtf8 (Text.pack value))
    encode _ = error "encodeUtf8: the checker passed a non-String"
    byteLength (VBytes value) = VInt (BS.length value)
    byteLength _ = error "byteLength: the checker passed non-Bytes"
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
