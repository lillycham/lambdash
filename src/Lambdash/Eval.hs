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
    methodValue,
    constructorValue,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Map.Lazy qualified as Map.Lazy
import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.List (isInfixOf, isPrefixOf)
import Data.IORef
import Control.Concurrent.STM
import Control.Exception (IOException, SomeException, bracket, displayException, fromException, throwIO, try)
import Control.Monad (forM_, void)
import System.Posix.Signals (Handler (..), installHandler, sigINT)
import Data.List (dropWhileEnd, intercalate, sort, sortBy, transpose)
import Lambdash.Classes (extendFields, fromIntName, instanceName, recordFields, superField)
import System.Directory (doesDirectoryExist, getCurrentDirectory, getFileSize, getHomeDirectory, listDirectory, pathIsSymbolicLink, setCurrentDirectory)
import System.Environment (lookupEnv, setEnv)
import Control.Monad (when)
import Lambdash.Infer (Arg (..), Env, Expr (..), Lit (..), Name, Scheme (..), mono)
import Lambdash.Syntax (Pattern (..))
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
  | -- | A value of a declared data type: its constructor, and its fields.
    VData Name [Value]
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
render (VData name fields) = unwords (name : map field fields)
  where
    field value@(VData _ (_ : _)) = "(" <> render value <> ")"
    field value@(VInt n) | n < 0 = "(" <> render value <> ")"
    field value = render value
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
    -- Lazy, so that an instance's dictionary can refer to itself.
    ELet name value body -> eval (Map.Lazy.insert name (eval env value) env) body
    -- Each closure captures the scope that holds them all. Every bound
    -- expression is a lambda, so building one forces nothing.
    ELetRec bindings body ->
      let env' = foldr (\(name, value) -> Map.Lazy.insert name (eval env' value)) env bindings
      in eval env' body
    ECmd name args -> VStream [External name (map word args)] ReturnStatus
    ELoc _ e -> eval env e
    EAnn e _ -> eval env e
    ECase scrutinee arms ->
      let value = eval env scrutinee
      in case [(binds, body) | (p, body) <- arms, Just binds <- [match p value]] of
        (binds, body) : _ -> eval (foldr (uncurry Map.insert) env binds) body
        [] -> error "eval: no arm matched, and the checker said one would"
    EIf c a b -> case eval env c of
      VBool True -> eval env a
      VBool False -> eval env b
      _ -> error "eval: the checker passed a non-Bool condition"
    EBind name e body -> VComp $ \runtime -> do
      value <- runComp runtime (eval env e)
      runComp runtime (eval (Map.insert name value env) body)
    ERun e -> VComp (\runtime -> runComp runtime (eval env e))
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

-- | The names a pattern binds, if it matches the value.
match :: Pattern -> Value -> Maybe [(Name, Value)]
match p value = case (p, value) of
  (PWild, _) -> Just []
  (PVar name, _) -> Just [(name, value)]
  (PCon name fields, VData name' values)
    | name == name' && length fields == length values -> concat <$> sequence (zipWith match fields values)
  (PLit (LInt n), VInt m) | n == m -> Just []
  (PLit (LBool b), VBool c) | b == c -> Just []
  (PLit LUnit, VUnit) -> Just []
  (PLit (LStr s), VStr t) | s == t -> Just []
  (PRecord fields, VRecord values) ->
    concat <$> traverse (\(label, field) -> lookup label values >>= match field) fields
  _ -> Nothing

-- | A constructor with this many fields: a function of each field, in
-- turn, or the value itself when it has none.
constructorValue :: Name -> Int -> Value
constructorValue name arity = go arity []
 where
  go 0 fields = VData name (reverse fields)
  go k fields = VBuiltin name (\field -> go (k - 1) (field : fields))

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
typeEnv = Map.fromList [(n, t) | (n, t, _) <- builtins <> aliases <> methods]

-- | The builtins as the evaluator sees them.
scope :: Scope
scope = Map.fromList ([(n, x) | (n, _, x) <- builtins <> aliases <> methods] <> dictionaries)

-- | The class methods. Each takes its class's dictionary first: the checker
-- elaborates @eq x y@ to @eq d x y@, with @d@ chosen by type.
methods :: [(Name, Scheme, Value)]
methods =
  [ method "eq" "Eq" (\a -> TFun a (TFun a TBool))
  , method "ne" "Eq" (\a -> TFun a (TFun a TBool))
  , method "lt" "Ord" (\a -> TFun a (TFun a TBool))
  , method "le" "Ord" (\a -> TFun a (TFun a TBool))
  , method "gt" "Ord" (\a -> TFun a (TFun a TBool))
  , method "ge" "Ord" (\a -> TFun a (TFun a TBool))
  , method "min" "Ord" (\a -> TFun a (TFun a a))
  , method "max" "Ord" (\a -> TFun a (TFun a a))
  -- Operators are ordinary names: a == b desugars to (==) a b.
  , alias "==" "Eq" "eq" (\a -> TFun a (TFun a TBool))
  , alias "!=" "Eq" "ne" (\a -> TFun a (TFun a TBool))
  , alias "<" "Ord" "lt" (\a -> TFun a (TFun a TBool))
  , alias "<=" "Ord" "le" (\a -> TFun a (TFun a TBool))
  , alias ">" "Ord" "gt" (\a -> TFun a (TFun a TBool))
  , alias ">=" "Ord" "ge" (\a -> TFun a (TFun a TBool))
  , method "+" "Num" (\a -> TFun a (TFun a a))
  , method "-" "Num" (\a -> TFun a (TFun a a))
  , method "*" "Num" (\a -> TFun a (TFun a a))
  , method "negate" "Num" (\a -> TFun a a)
  , method "/" "Integral" (\a -> TFun a (TFun a a))
  , method "%" "Integral" (\a -> TFun a (TFun a a))
  , alias "div" "Integral" "/" (\a -> TFun a (TFun a a))
  , alias "mod" "Integral" "%" (\a -> TFun a (TFun a a))
  , method "<>" "Semigroup" (\a -> TFun a (TFun a a))
  , method "show" "Show" (\a -> TFun a TString)
  , method "display" "Display" (\a -> TFun a TString)
  -- Each field of a record, in the record's order, as a person reads it.
  , ( "fields"
    , Forall [Var 0] (TClass [IsIn "Display" (TRecord (RVar (Var 0)))]
        (TFun (TRecord (RVar (Var 0))) (TList (TRecord (RExtend "label" TString (RExtend "value" TString REmpty))))))
    , VBuiltin "fields" (\d -> VBuiltin "fields" (\r -> case r of
        VRecord values -> VList [VRecord [("label", VStr l), ("value", VStr (cell d l v))] | (l, v) <- values]
        _ -> error "fields: the checker passed a non-record")) )
  -- A table of every record that arrives: it reads them all first, to fit
  -- the widths. The first record gives the columns and their order.
  , ( "table"
    , Forall [Var 0] (TClass [IsIn "Display" (TRecord (RVar (Var 0)))] (TStage (TRecord (RVar (Var 0))) TBytes))
    , VBuiltin "table" (\d -> VStage [TransformStage (tableStage d)]) )
  -- What the prompt shows a value through, whatever the session binds.
  , alias "%show" "Show" "show" (\a -> TFun a TString)
  -- For the prelude's default methods, which no session can rebind.
  , alias "%eq" "Eq" "eq" (\a -> TFun a (TFun a TBool))
  , alias "%lt" "Ord" "lt" (\a -> TFun a (TFun a TBool))
  , ("%if", Forall [Var 0] (TFun TBool (TFun (TVar (Var 0)) (TFun (TVar (Var 0)) (TVar (Var 0))))),
      VBuiltin "%if" (\c -> VBuiltin "%if" (\t -> VBuiltin "%if" (\f -> if truth c then t else f))))
  , (fromIntName, Forall [Var 0] (TClass [IsIn "Num" (TVar (Var 0))] (TFun TInt (TVar (Var 0)))), VBuiltin fromIntName (select "fromInt"))
  -- Lazy in the second argument, as the evaluator is.
  , ("&&", mono (TFun TBool (TFun TBool TBool)), fn2 "&&" (\a b -> if truth a then b else VBool False))
  , ("||", mono (TFun TBool (TFun TBool TBool)), fn2 "||" (\a b -> if truth a then VBool True else b))
  , ( ">>>", Forall [Var 0, Var 1, Var 2] (TFun (TFun (TVar (Var 0)) (TVar (Var 1))) (TFun (TFun (TVar (Var 1)) (TVar (Var 2))) (TFun (TVar (Var 0)) (TVar (Var 2)))))
    , fn2 ">>>" (\f g -> VBuiltin ">>>" (applyValue g . applyValue f)) )
  , ( "<<<", Forall [Var 0, Var 1, Var 2] (TFun (TFun (TVar (Var 1)) (TVar (Var 2))) (TFun (TFun (TVar (Var 0)) (TVar (Var 1))) (TFun (TVar (Var 0)) (TVar (Var 2)))))
    , fn2 "<<<" (\g f -> VBuiltin "<<<" (applyValue g . applyValue f)) )
  , ( "sort"
    , Forall [Var 0] (TClass [IsIn "Ord" (TVar (Var 0))] (TFun (TList (TVar (Var 0))) (TList (TVar (Var 0)))))
    , VBuiltin "sort" (\d -> VBuiltin "sort" (\xs -> VList (sortWith d id (items xs)))) )
  , ( "sortOn"
    , Forall [Var 0, Var 1] (TClass [IsIn "Ord" (TVar (Var 1))]
        (TFun (TFun (TVar (Var 0)) (TVar (Var 1))) (TFun (TList (TVar (Var 0))) (TList (TVar (Var 0))))))
    , VBuiltin "sortOn" (\d -> VBuiltin "sortOn" (\key -> VBuiltin "sortOn" (\xs -> VList (sortWith d (applyValue key) (items xs))))) )
  ]
 where
  method name cls shape = alias name cls name shape
  tableStage d receive emit = do
    let drain sofar = receive >>= maybe (pure (reverse sofar)) (\row -> drain (row : sofar))
    rows <- drain []
    case rows of
      VRecord first : _ -> do
        let labels = map fst first
            grid = [[cell d l (select l row) | l <- labels] | row <- rows]
            widths = map (maximum . map length) (transpose (labels : grid))
            -- Trimmed, so that an empty last cell leaves no trailing space.
            line cells = dropWhileEnd (== ' ') (intercalate "  " (zipWith pad widths cells))
            pad width text = text <> replicate (width - length text) ' '
            out text = emit (VBytes (Text.encodeUtf8 (Text.pack (text <> "\n"))))
        mapM_ (out . line) (labels : map (`replicate` '-') widths : grid)
      _ -> pure ()
  alias name cls field shape =
    (name, Forall [Var 0] (TClass [IsIn cls (TVar (Var 0))] (shape (TVar (Var 0)))), VBuiltin name (select field))
  items (VList xs) = xs
  items _ = error "sort: the checker passed a non-list"
  -- A stable sort by the dictionary's lt.
  sortWith d key = sortBy (\a b -> compareBy d (key a) (key b))
  compareBy d a b
    | truth (call2 (select "lt" d) a b) = LT
    | truth (call2 (select "lt" d) b a) = GT
    | otherwise = EQ

-- | The prelude's instances, under the names the checker refers to.
dictionaries :: [(Name, Value)]
dictionaries =
  [(instanceName "Eq" key, eqDictionary structuralEq) | key <- ["Int", "Bool", "String", "()", "Status", "Bytes", "Patch", "Overlay"]]
    <> [(instanceName "Ord" key, ordDictionary (eqDictionary structuralEq) structuralLt) | key <- ["Int", "Bool", "String", "()"]]
    <> [(instanceName "Show" key, showDictionary render) | key <- ["Int", "Bool", "String", "()", "Status", "Bytes", "Patch", "Overlay"]]
    <> [(instanceName "Display" key, displayDictionary plain) | key <- ["Int", "Bool", "String", "()", "Status", "Bytes", "Patch", "Overlay"]]
    <> [ (instanceName "Display" "[]", VBuiltin "Display List" (\d -> displayDictionary (\v -> case v of
           VList xs -> "[" <> commaSep [displayed d x | x <- xs] <> "]"
           _ -> error "Display List: the checker passed a non-list")))
       -- As for Show: each field through its own dictionary.
       , (instanceName "Display" "{}", VBuiltin "Display Record" (\fields -> case displayDictionary (\v -> case v of
           VRecord values -> "{" <> commaSep [label <> " = " <> cell (VRecord [(recordFields, fields)]) label x | (label, x) <- values] <> "}"
           _ -> error "Display Record: the checker passed a non-record") of
             VRecord methods' -> VRecord ((recordFields, fields) : methods')
             other -> other)) ]
    <> [ (instanceName "Num" "Int", numInt)
       , (instanceName "Integral" "Int", VRecord
           [ (superField "Num", numInt)
           , ("/", intOp "/" div)
           , ("%", intOp "%" mod) ])
       , (instanceName "Semigroup" "String", semigroup (\a b -> case (a, b) of (VStr x, VStr y) -> VStr (x <> y); _ -> error "<>: String"))
       , (instanceName "Semigroup" "Bytes", semigroup (\a b -> case (a, b) of (VBytes x, VBytes y) -> VBytes (x <> y); _ -> error "<>: Bytes"))
       , (instanceName "Semigroup" "[]", semigroup (\a b -> case (a, b) of (VList x, VList y) -> VList (x <> y); _ -> error "<>: List"))
       -- One overlay, whose later patches win, as use would apply them.
       , (instanceName "Semigroup" "Overlay", semigroup (\a b -> case (a, b) of
           (VOverlay (Overlay x px), VOverlay (Overlay y py)) -> VOverlay (Overlay (x <> "+" <> y) (px <> py))
           _ -> error "<>: Overlay"))
       , (instanceName "Eq" "[]", VBuiltin "Eq List" listEq)
       , (instanceName "Ord" "[]", VBuiltin "Ord List" listOrd)
       , (instanceName "Eq" "{}", VBuiltin "Eq Record" recordEq)
       , (instanceName "Show" "[]", VBuiltin "Show List" (\d -> showDictionary (\v -> case v of
           VList xs -> "[" <> commaSep [shown d x | x <- xs] <> "]"
           _ -> error "Show List: the checker passed a non-list")))
       -- A record shows each field through its own dictionary, so that a
       -- declared instance for a field's type shows there too.
       , (instanceName "Show" "{}", VBuiltin "Show Record" (\fields -> case fields of
           VRecord ds -> case showDictionary (\v -> case v of
               VRecord values -> "{" <> commaSep [label <> " = " <> maybe (render x) (`shown` x) (lookup label ds) | (label, x) <- values] <> "}"
               _ -> error "Show Record: the checker passed a non-record") of
             VRecord methods' -> VRecord ((recordFields, fields) : methods')
             other -> other
           _ -> error "Show Record: the elaborator passed a non-record"))
       , (extendFields, fn2 extendFields extend) ]
 where
  numInt = VRecord
    [ ("+", intOp "+" (+))
    , ("-", intOp "-" (-))
    , ("*", intOp "*" (*))
    , ("negate", VBuiltin "negate" (\v -> case v of VInt n -> VInt (negate n); _ -> error "negate: Int"))
    , ("fromInt", VBuiltin "fromInt" id)
    ]
  intOp name op = fn2 name (\a b -> case (a, b) of
    (VInt x, VInt y) -> VInt (op x y)
    _ -> error (name <> ": the checker passed a non-Int"))
  semigroup combine = VRecord [("<>", fn2 "<>" combine)]
  listEq d = eqDictionary (listEquals d)
  -- A record's dictionary keeps its field dictionaries, so that a function
  -- over an open row can extend them with the fields it knows.
  recordEq fields@(VRecord dictionaries') = case eqDictionary (recordEquals dictionaries') of
    VRecord methods' -> VRecord ((recordFields, fields) : methods')
    other -> other
  recordEq _ = error "Eq Record: the elaborator passed a non-record"
  recordEquals dictionaries' a b =
    and [truth (call2 (select "eq" d) (select label a) (select label b)) | (label, d) <- dictionaries']
  extend (VRecord new) (VRecord old) = VRecord (new <> old)
  extend _ _ = error "extendFields: the elaborator passed a non-record"
  listOrd d = ordDictionary (listEq (select (superField "Eq") d)) (listLess d)
  listEquals d (VList xs) (VList ys) =
    length xs == length ys && and (zipWith (\x y -> truth (call2 (select "eq" d) x y)) xs ys)
  listEquals _ _ _ = error "Eq List: the checker passed a non-list"
  -- Lexicographic, as in Haskell.
  listLess d (VList xs) (VList ys) = go xs ys
   where
    go [] [] = False
    go [] _ = True
    go _ [] = False
    go (x : xt) (y : yt)
      | truth (call2 (select "lt" d) x y) = True
      | truth (call2 (select "lt" d) y x) = False
      | otherwise = go xt yt
  listLess _ _ _ = error "Ord List: the checker passed a non-list"

-- | A dictionary for Show from a way to show.
showDictionary :: (Value -> String) -> Value
showDictionary f = VRecord [("show", VBuiltin "show" (VStr . f))]

-- | A dictionary for Display from a way to display.
displayDictionary :: (Value -> String) -> Value
displayDictionary f = VRecord [("display", VBuiltin "display" (VStr . f))]

-- | A value displayed through a Display dictionary.
displayed :: Value -> Value -> String
displayed d x = case applyValue (select "display" d) x of
  VStr text -> text
  _ -> error "display: a method passed a non-String"

-- | A value for a person: a String without its quotes.
plain :: Value -> String
plain (VStr s) = s
plain value = render value

-- | One field of a record, through the field's dictionary in a record's
-- Display dictionary.
cell :: Value -> Label -> Value -> String
cell (VRecord methods') label value
  | Just (VRecord ds) <- lookup recordFields methods', Just d <- lookup label ds = displayed d value
cell _ _ value = plain value

-- | A value shown through a Show dictionary.
shown :: Value -> Value -> String
shown d x = case applyValue (select "show" d) x of
  VStr text -> text
  _ -> error "show: a method passed a non-String"

-- | A method: the field of its class's dictionary.
methodValue :: Name -> Value
methodValue field = VBuiltin field (select field)

-- | A dictionary for Eq from an equality.
eqDictionary :: (Value -> Value -> Bool) -> Value
eqDictionary equals = VRecord
  [ ("eq", fn2 "eq" (\a b -> VBool (equals a b)))
  , ("ne", fn2 "ne" (\a b -> VBool (not (equals a b))))
  ]

-- | A dictionary for Ord from its Eq dictionary and an order.
ordDictionary :: Value -> (Value -> Value -> Bool) -> Value
ordDictionary eqD less = VRecord
  [ (superField "Eq", eqD)
  , ("lt", fn2 "lt" (\a b -> VBool (less a b)))
  , ("le", fn2 "le" (\a b -> VBool (not (less b a))))
  , ("gt", fn2 "gt" (\a b -> VBool (less b a)))
  , ("ge", fn2 "ge" (\a b -> VBool (not (less a b))))
  , ("min", fn2 "min" (\a b -> if less b a then b else a))
  , ("max", fn2 "max" (\a b -> if less a b then b else a))
  ]

fn2 :: Name -> (Value -> Value -> Value) -> Value
fn2 name f = VBuiltin name (\a -> VBuiltin name (f a))

call2 :: Value -> Value -> Value -> Value
call2 f a b = applyValue (applyValue f a) b

truth :: Value -> Bool
truth (VBool b) = b
truth _ = error "a method passed a non-Bool"

-- | Equality on the values of the built-in Eq types.
structuralEq :: Value -> Value -> Bool
structuralEq a b = case (a, b) of
  (VInt x, VInt y) -> x == y
  (VBool x, VBool y) -> x == y
  (VUnit, VUnit) -> True
  (VStr x, VStr y) -> x == y
  (VStatus x, VStatus y) -> x == y
  (VBytes x, VBytes y) -> x == y
  (VPatch x, VPatch y) -> x == y
  (VOverlay x, VOverlay y) -> x == y
  _ -> error "Eq: the checker passed values of different types"

-- | The order on the values of the built-in Ord types.
structuralLt :: Value -> Value -> Bool
structuralLt a b = case (a, b) of
  (VInt x, VInt y) -> x < y
  (VBool x, VBool y) -> x < y
  (VUnit, VUnit) -> False
  (VStr x, VStr y) -> x < y
  _ -> error "Ord: the checker passed values of different types"

-- | The builtins that sugar refers to, under names that no identifier can
-- spell, so that rebinding @use@ or @pkg@ does not change what @use REF@
-- means. See "Lambdash.Desugar".
aliases :: [(Name, Scheme, Value)]
aliases = [('%' : n, t, x) | (n, t, x) <- builtins, n `elem` ["pkg", "use", "useLocal", "not"]]
