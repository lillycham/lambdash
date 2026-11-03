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
    summarise,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.List (isInfixOf, isPrefixOf)
import Data.IORef
import Control.Concurrent.STM
import Control.Exception (IOException, SomeException, bracket, displayException, fromException, throwIO, try)
import Control.Monad (forM_, void)
import System.Posix.Signals (Handler (..), installHandler, sigINT)
import Data.List (sort)
import System.Directory (doesDirectoryExist, getCurrentDirectory, getFileSize, getHomeDirectory, listDirectory, pathIsSymbolicLink, setCurrentDirectory)
import System.Environment (lookupEnv, setEnv)
import Control.Monad (when)
import Lambdash.Infer (Arg (..), Env, Expr (..), Lit (..), Name, Scheme (..), Stmt (..), mono)
import Lambdash.Nix (resolveDev, resolvePkg)
import Lambdash.Overlay
import Lambdash.Runtime
import Lambdash.Process (Status (..))
import Lambdash.Stream (StreamError (..))
import Lambdash.Stream (Stage (..), runStagesAll)
import Lambdash.Lines (runLines)
import Lambdash.Unify
import System.IO (hPutStrLn, stderr, stdout)

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
  | -- | An action. Building one runs nothing: only 'runComp' does. It gets the
    -- session's environments when it runs, not when it is built.
    VComp (Runtime -> IO Value)
  | VOverlay Overlay
  | VPatch Patch
  | -- | A job handle: the job, and where its result arrives.
    VJob Job (TMVar (Either SomeException Value))
  | VStream [Stage Value] Return
  | VStage [Stage Value]
  | VCollector [Stage Value]

-- | What a plan returns. 'ReturnPipefail' is the status of the first stage
-- that failed, where a stage killed by SIGPIPE before the last one did not
-- fail: a closed reader is how a pipeline cancels its producers.
data Return = ReturnUnit | ReturnStatus | ReturnPipefail

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
render (VOverlay overlay) = renderOverlay overlay
render (VPatch patch) = renderPatch patch
render (VJob job _) = "<job " <> show (jobNumber job) <> ">"

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
runComp :: Runtime -> Value -> IO Value
runComp runtime (VComp action) = action runtime
runComp runtime (VStream stages result) = runPlan runtime result stages output
 where
  output (VBytes value) = BS.hPut stdout value
  output (VStr value) = putStrLn value
  output value = putStrLn (render value)
runComp _ _ = error "runComp: the checker passed a non-computation"

bytes :: Value -> BS.ByteString
bytes (VBytes value) = value
bytes _ = error "stream: the checker passed non-Bytes to an external"

runPlan :: Runtime -> Return -> [Stage Value] -> (Value -> IO ()) -> IO Value
runPlan runtime result stages output = fst <$> runPlanAll runtime result stages output

-- | The return value, and the status of each external in order.
runPlanAll :: Runtime -> Return -> [Stage Value] -> (Value -> IO ()) -> IO (Value, [Status])
runPlanAll runtime result stages output = do
  statuses <- runStagesAll runtime VBytes bytes output stages
  let value = case result of
        ReturnStatus -> VStatus (lastStatus statuses)
        ReturnPipefail -> VStatus (pipefailStatus statuses)
        ReturnUnit -> VUnit
  pure (value, statuses)

lastStatus :: [Status] -> Status
lastStatus statuses = case reverse statuses of
  status : _ -> status
  [] -> ExitedWith 0

-- | The first failure, or the last status when nothing failed.
pipefailStatus :: [Status] -> Status
pipefailStatus statuses =
  case [status | (index, status) <- zip [1 :: Int ..] statuses, failedAt index status] of
    status : _ -> status
    [] -> lastStatus statuses
 where
  count = length statuses
  failedAt _ (ExitedWith 0) = False
  failedAt index (KilledBy 13) = index == count
  failedAt _ _ = True

collectPlan :: Return -> [Stage Value] -> Value
collectPlan result stages = VComp $ \runtime -> do
  values <- newIORef []
  -- The final bridge routes even a raw external's stdout through the sink.
  (status, statuses) <- runPlanAll runtime result (stages <> [MapStage id]) (\value -> modifyIORef' values (value :))
  items <- reverse <$> readIORef values
  pure (VRecord [("items", VList items), ("status", status), ("statuses", VList (map VStatus statuses))])

pipeValue :: Value -> Value -> Value
pipeValue (VStream left result) (VCollector right) = collectPlan result (left <> right)
pipeValue (VStage left) (VCollector right) = VCollector (left <> right)
pipeValue (VComp action) (VCollector right) = collectPlan ReturnUnit (SourceStage action : right)
pipeValue value (VCollector right) = collectPlan ReturnUnit (SourceStage (const (pure value)) : right)
pipeValue (VStream left result) (VStage right) = VStream (left <> right) result
pipeValue (VStage left) (VStage right) = VStage (left <> right)
pipeValue (VComp action) (VStage right) = VStream (SourceStage action : right) ReturnUnit
pipeValue value (VStage right) = VStream (SourceStage (const (pure value)) : right) ReturnUnit
pipeValue (VStage left) function = VStage (left <> [MapStage (applyValue function)])
pipeValue (VStream left _) (VStream right result) = VStream (left <> right) result
pipeValue (VStream left result) function = VStream (left <> [MapStage (applyValue function)]) result
pipeValue (VComp action) (VStream right result) = VStream (SourceStage action : right) result
pipeValue value (VStream right result) = VStream (SourceStage (const (pure value)) : right) result
pipeValue (VComp action) function = VComp (fmap (applyValue function) . action)
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
    EDo stmts -> VComp (\runtime -> runStmts runtime env stmts)
    EBg e -> VComp (\runtime -> background runtime (eval env e))
    EWith overlay body -> case eval env overlay of
      VOverlay layer -> VComp $ \runtime -> do
        warnShadows runtime layer
        runComp runtime {runtimeLocal = runtimeLocal runtime <> [layer]} (eval env body)
      _ -> error "eval: the checker passed a non-Overlay to with"
  where
    evalLit (LInt n) = VInt n
    evalLit (LBool b) = VBool b
    evalLit LUnit = VUnit
    evalLit (LStr s) = VStr s
    evalLit (LRef s) = VStr s
    word (AWord s) = s
    word (AVar name) = case Map.lookup name env of
      Just (VStr s) -> s
      _ -> error "eval: the checker passed a non-String argument"

-- | What @ls@ gives for each entry. @ext@ is the part after the last dot, or
-- empty.
entryTy :: Ty
entryTy = TRecord (RExtend "name" TString (RExtend "type" TString (RExtend "size" TInt (RExtend "ext" TString REmpty))))

expandPath :: String -> IO FilePath
expandPath "~" = getHomeDirectory
expandPath ('~' : '/' : rest) = (<> "/" <> rest) <$> getHomeDirectory
expandPath path = pure path

-- | @cd path@. @cd -@ goes back. @PWD@ and @OLDPWD@ follow, as in other
-- shells, and later spawns inherit the new directory.
--
-- Note: this changes the process at once, so a line that fails after a @cd@
-- keeps the new directory.
changeDirectory :: String -> IO ()
changeDirectory path = do
  old <- getCurrentDirectory
  target <- if path == "-" then maybe old id <$> lookupEnv "OLDPWD" else expandPath path
  result <- try (setCurrentDirectory target) :: IO (Either IOException ())
  case result of
    Left _ -> throwIO (StreamError ("cd: no such directory: " <> path))
    Right () -> do
      new <- getCurrentDirectory
      setEnv "OLDPWD" old
      setEnv "PWD" new

listEntries :: String -> IO [Value]
listEntries path = do
  directory <- expandPath path
  listed <- try (listDirectory directory) :: IO (Either IOException [FilePath])
  names <- either (const (throwIO (StreamError ("ls: cannot read: " <> path)))) (pure . sort) listed
  mapM (entry directory) names
 where
  entry directory name = do
    let full = directory <> "/" <> name
    link <- pathIsSymbolicLink full
    isDirectory <- doesDirectoryExist full
    size <- if isDirectory then pure 0 else either (const 0) fromIntegral <$> (try (getFileSize full) :: IO (Either IOException Integer))
    let kind | link = "link" | isDirectory = "dir" | otherwise = "file"
        ext = case break (== '.') (reverse name) of
          (rest, '.' : stem) | not (null stem) && not (null rest) -> reverse rest
          _ -> ""
    pure (VRecord [("name", VStr name), ("type", VStr kind), ("size", VInt size), ("ext", VStr ext)])

-- | @e &@: start the action as a job that does not own the terminal, register
-- it for the REPL's table, and give its handle at once.
background :: Runtime -> Value -> IO Value
background runtime action = do
  number <- case runtimeTable runtime of
    Nothing -> pure 0
    Just table -> atomicModifyIORef' (tableNext table) (\n -> (n + 1, n))
  (job, result) <- startJob runtime number (runtimeLine runtime) False (`runComp` action) summarise
  forM_ (runtimeTable runtime) (\table -> modifyIORef' (tableNew table) (job :))
  pure (VJob job result)

-- | What a job's notice says when it ends: what the line would have shown.
summarise :: Either SomeException Value -> Either String String
summarise (Right VUnit) = Right ""
summarise (Right (VStatus (ExitedWith 0))) = Right ""
summarise (Right value) = Right (render value)
summarise (Left err) = Left (maybe (displayException err) (\(StreamError message) -> message) (fromException err))

-- | The result of a job, once it is done. A stopped job cannot finish by
-- itself, so waiting for one is an error rather than a hang.
--
-- The shell ignores SIGINT so that Ctrl-C reaches jobs. While wait blocks,
-- the shell owns the terminal and no job does, so wait catches SIGINT itself.
waitFor :: Job -> TMVar (Either SomeException Value) -> IO Value
waitFor job result = do
  interrupted <- newEmptyTMVarIO
  let catchInt = Catch (atomically (void (tryPutTMVar interrupted ())))
      waitOn = atomically $
            (Right . Just <$> readTMVar result)
        `orElse` (readTVar (jobStopped job) >>= check >> pure (Right Nothing))
        `orElse` (Left () <$ takeTMVar interrupted)
  outcome <- bracket (installHandler sigINT catchInt Nothing) (\old -> installHandler sigINT old Nothing) (const waitOn)
  case outcome of
    Left () -> throwIO (StreamError ("wait: interrupted; job " <> show (jobNumber job) <> " is still running"))
    Right Nothing -> throwIO (StreamError ("job " <> show (jobNumber job) <> " is stopped: fg it"))
    Right (Just value) -> markCollected job >> either throwIO pure value

-- | Bring a job to the foreground, then wait for it.
foregroundJob :: Job -> TMVar (Either SomeException Value) -> IO Value
foregroundJob job result = do
  continueJob True job
  outcome <- awaitDone job (readTMVar result)
  case outcome of
    Nothing -> throwIO (StreamError ("[" <> show (jobNumber job) <> "] stopped  " <> jobLine job))
    Just value -> markCollected job >> either throwIO pure value

-- | Run a block's statements in order. Each statement sees the names that
-- earlier ones bound.
runStmts :: Runtime -> Scope -> [Stmt] -> IO Value
runStmts _ _ [] = pure VUnit
runStmts runtime env [SRun e] = runComp runtime (eval env e)
runStmts runtime env (stmt : rest) = case stmt of
  SRun e -> runComp runtime (eval env e) >> runStmts runtime env rest
  SBind name e -> do
    value <- runComp runtime (eval env e)
    runStmts runtime (Map.insert name value env) rest
  SLet name e -> runStmts runtime (Map.insert name (eval env e) env) rest

-- | A @with@ block does not change the prompt, so it warns once about the
-- shadowing it causes.
warnShadows :: Runtime -> Overlay -> IO ()
warnShadows runtime layer = do
  state <- readIORef (runtimeState runtime)
  cwd <- getCurrentDirectory
  let outer = withLayers (runtimeLocal runtime) (inScope cwd state)
      inner = withLayers [layer] outer
      fresh = [shadow | shadow <- shadows inner, shadow `notElem` shadows outer]
  mapM_ (\(Shadow var earlier later) -> hPutStrLn stderr ("warning: " <> later <> " shadows " <> var <> " from " <> earlier)) fresh

-- | The names that exist before any input, with a type and a value together, so
-- that the two cannot drift apart.
builtins :: [(Name, Scheme, Value)]
builtins =
  [ ("not", mono (TFun TBool TBool), VBuiltin "not" notV),
    ("add", mono (TFun TInt (TFun TInt TInt)), VBuiltin "add" add),
    ("print", Forall [t] (TFun (TVar t) (TComp (effect Console) TUnit)), VBuiltin "print" printV),
    ("pure", Forall [t] (TFun (TVar t) (TComp noEffects (TVar t))), VBuiltin "pure" (\value -> VComp (const (pure value)))),
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
    ("encodeLine", mono (TFun TString TBytes), VBuiltin "encodeLine" encodeLine),
    ("set", mono (TFun TString (TFun TString TPatch)), string2 "set" (\name value -> VPatch (Patch name (Set value)))),
    ("unset", mono (TFun TString TPatch), VBuiltin "unset" (\name -> VPatch (Patch (str name) Unset))),
    ("prepend", mono (TFun TString (TFun TString TPatch)), string2 "prepend" (\name value -> VPatch (Patch name (Prepend ":" value)))),
    ("append", mono (TFun TString (TFun TString TPatch)), string2 "append" (\name value -> VPatch (Patch name (Append ":" value)))),
    ("prependWith", mono (TFun TString (TFun TString (TFun TString TPatch))), VBuiltin "prependWith" (\sep -> string2 "prependWith" (\name value -> VPatch (Patch name (Prepend (str sep) value))))),
    ("appendWith", mono (TFun TString (TFun TString (TFun TString TPatch))), VBuiltin "appendWith" (\sep -> string2 "appendWith" (\name value -> VPatch (Patch name (Append (str sep) value))))),
    ("overlay", mono (TFun TString (TFun (TList TPatch) TOverlay)), VBuiltin "overlay" overlayV),
    ("use", mono (TFun TOverlay (TComp (effect Environment) TUnit)), VBuiltin "use" useV),
    ("useLocal", mono (TFun TOverlay (TComp (effect Environment) TUnit)), VBuiltin "useLocal" useLocalV),
    ("unuse", mono (TFun TString (TComp (effect Environment) TUnit)), VBuiltin "unuse" unuseV),
    ( "wait",
      Forall [t, effA] (TFun (TJob (effectVar effA) (TVar t)) (TComp (effectVar effA) (TVar t))),
      VBuiltin "wait" (jobAction waitFor) ),
    ( "fg",
      Forall [t, effA] (TFun (TJob (effectVar effA) (TVar t)) (TComp (effectVar effA) (TVar t))),
      VBuiltin "fg" (jobAction foregroundJob) ),
    ( "pipefail",
      Forall [t, u, effA] (TFun (TStream (effectVar effA) (TVar t) (TVar u) TStatus) (TStream (effectVar effA) (TVar t) (TVar u) TStatus)),
      VBuiltin "pipefail" pipefailV ),
    ("cd", mono (TFun TString (TComp (effect WorkingDirectory) TUnit)), VBuiltin "cd" (\path -> VComp (const (VUnit <$ changeDirectory (str path))))),
    ("ls", mono (TFun TString (TStream (effect Filesystem) TBytes entryTy TUnit)), VBuiltin "ls" lsV),
    ("environments", mono (TComp (effect Environment) (TList TString)), VComp environmentsV),
    ("pkg", mono (TFun TString (TComp (effect (Exec "nix")) TOverlay)), VBuiltin "pkg" (\ref -> VComp (\runtime -> VOverlay <$> resolvePkg runtime (str ref)))),
    ("dev", mono (TFun TString (TComp (unionEffects (effect (Exec "nix")) (effect (Exec "bash"))) TOverlay)), VBuiltin "dev" (\ref -> VComp (\runtime -> VOverlay <$> resolveDev runtime (str ref)))),
    ( "andThen",
      Forall [t, u, effA, effB] (TFun (TComp (effectVar effA) (TVar t)) (TFun (TFun (TVar t) (TComp (effectVar effB) (TVar u))) (TComp (unionEffects (effectVar effA) (effectVar effB)) (TVar u)))),
      VBuiltin "andThen" andThenV )
  ]
  where
    t = Var 0
    u = Var 1
    effA = Var 2
    effB = Var 3
    lsV path = VStream [TransformStage (\_ emit -> listEntries (str path) >>= mapM_ emit)] ReturnUnit
    jobAction act (VJob job result) = VComp (const (act job result))
    jobAction _ _ = error "wait: the checker passed a non-job"
    pipefailV (VStream stages _) = VStream stages ReturnPipefail
    pipefailV _ = error "pipefail: the checker passed a non-stream"
    -- Run the first, then the computation that its result chooses.
    andThenV first = VBuiltin "andThen" $ \next -> VComp $ \runtime -> do
      value <- runComp runtime first
      runComp runtime (applyValue next value)
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
    printV (VStr s) = VComp (const (VUnit <$ putStrLn s))
    printV value = VComp (const (VUnit <$ putStrLn (render value)))
    str (VStr text) = text
    str _ = error "overlay: the checker passed a non-String"
    string2 name f = VBuiltin name (\a -> VBuiltin name (\b -> f (str a) (str b)))
    overlayV name = VBuiltin "overlay" $ \patches -> case patches of
      VList items -> VOverlay (Overlay (str name) [patch | VPatch patch <- items])
      _ -> error "overlay: the checker passed a non-list"
    -- The change goes to the runtime's reference. The REPL keeps it only if
    -- the whole line succeeds, so a failure later in the line undoes it.
    useV (VOverlay overlay) = VComp $ \runtime -> do
      modifyIORef' (runtimeState runtime) (useOverlay overlay)
      pure VUnit
    useV _ = error "use: the checker passed a non-Overlay"
    -- Active in this directory and below, for the rest of the session.
    useLocalV (VOverlay overlay) = VComp $ \runtime -> do
      cwd <- getCurrentDirectory
      modifyIORef' (runtimeState runtime) (useOverlayIn cwd overlay)
      pure VUnit
    useLocalV _ = error "useLocal: the checker passed a non-Overlay"
    unuseV name = VComp $ \runtime -> do
      state <- readIORef (runtimeState runtime)
      case unuseOverlay (str name) state of
        Left message -> throwIO (StreamError message)
        Right next -> VUnit <$ writeIORef (runtimeState runtime) next
    environmentsV runtime = do
      state <- readIORef (runtimeState runtime)
      pure (VList (map (VStr . overlayName) (overlays state)))
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
