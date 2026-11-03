-- | Bounded streams. Adjacent external stages share an OS pipe. A pure stage
-- uses a worker and one item of buffering between two pure stages. Closing a
-- downstream reader releases blocked writers and propagates cancellation.
module Lambdash.Stream (Stage (..), StreamError (..), runStages, runStagesAll) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently, mapConcurrently_)
import Control.Concurrent.STM
import Control.Exception hiding (handle)
import Control.Monad (forM, forM_, void, when)
import Data.ByteString qualified as BS
import Data.IORef
import Data.Map.Strict qualified as Map
import Lambdash.Overlay (Environ)
import Lambdash.Runtime (JobHooks (..), Runtime (..), runtimeEnviron)
import Lambdash.Process (Status (..), programExists, spawnStage)
import System.Exit (ExitCode (..))
import System.IO
import System.IO.Error (isResourceVanishedError)
import System.Posix.IO
import System.Posix.Process
import System.Posix.Signals
import System.Posix.Terminal
import System.Posix.Types

data Stage a
  = External String [String]
  | MapStage (a -> a)
  | -- | A source runs with the session, because it can be any computation.
    SourceStage (Runtime -> IO a)
  | SourceListStage [a]
  | TransformStage (IO (Maybe a) -> (a -> IO ()) -> IO ())

-- Expected data errors cross the worker boundary after resource cleanup.
data StreamError = StreamError String deriving (Eq, Show)
instance Exception StreamError

data Input a = RawInput Fd (IO ()) | Input (IO (Maybe a)) (IO ())
data Output a = RawOutput Fd (IO ()) | Output (a -> IO ()) (IO ())

data PipeClosed = PipeClosed deriving (Show)
instance Exception PipeClosed

quietly :: IO () -> IO ()
quietly action = void (try action :: IO (Either IOException ()))

-- The returned close action is safe to call both from a worker and on unwind.
managed :: IORef [IO ()] -> IO () -> IO (IO ())
managed resources close = do
  open <- newIORef True
  let release = mask_ $ do
        wasOpen <- atomicModifyIORef' open (\old -> (False, old))
        when wasOpen (quietly close)
  modifyIORef' resources (release :)
  pure release

withResources :: (IORef [IO ()] -> IO a) -> IO a
withResources = bracket (newIORef []) (\resources -> readIORef resources >>= sequence_)

-- Reap every child on every path, including spawn failure and async exceptions.
withChildren :: ((IORef [ProcessID], IORef (Maybe ProcessGroupID)) -> IO a) -> IO a
withChildren = bracketOnError acquire $ \(children, groupRef) -> do
  group <- readIORef groupRef
  forM_ group (quietly . signalProcessGroup sigKILL)
  live <- readIORef children
  forM_ live (quietly . signalProcess sigKILL)
  forM_ live (\pid -> quietly (void (getProcessStatus True False pid)))
 where
  acquire = (,) <$> newIORef [] <*> newIORef Nothing

isExternal :: Stage a -> Bool
isExternal (External _ _) = True
isExternal _ = False

-- Raw endpoints are inherited only by their own child, through dup2. CLOEXEC
-- prevents every other child from holding an extra writer and hiding EOF.
edge :: IORef [IO ()] -> (BS.ByteString -> a) -> (a -> BS.ByteString)
     -> Stage a -> Stage a -> IO (Output a, Input a)
edge resources fromBytes toBytes left right
  | not (isExternal left || isExternal right) = do
      queue <- newTBQueueIO 1
      ended <- newTVarIO False
      closed <- newTVarIO False
      let readItem = atomically $ do
            item <- tryReadTBQueue queue
            case item of
              Just value -> pure (Just value)
              Nothing -> do
                eof <- readTVar ended
                if eof then pure Nothing else retry
          writeItem value = atomically $ do
            shut <- readTVar closed
            when shut (throwSTM PipeClosed)
            writeTBQueue queue value
      pure (Output writeItem (atomically (writeTVar ended True)),
            Input readItem (atomically (writeTVar closed True)))
  | otherwise = mask_ $ do
      (r, w) <- createPipe
      (do
        setFdOption r CloseOnExec True
        setFdOption w CloseOnExec True)
        `onException` (closeFd r >> closeFd w)
      -- Register ownership before any interruptible IO. After fdToHandle,
      -- the handle owns its fd, so its finalizer must use hClose.
      readOwner <- newIORef (closeFd r)
      writeOwner <- newIORef (closeFd w)
      closeRead <- managed resources (readIORef readOwner >>= id)
      closeWrite <- managed resources (readIORef writeOwner >>= id)
      input <- if isExternal right
        then pure (RawInput r closeRead)
        else do
          handle <- fdToHandle r
          writeIORef readOwner (hClose handle)
          hSetBinaryMode handle True
          pure (Input (readChunk fromBytes handle) closeRead)
      output <- if isExternal left
        then pure (RawOutput w closeWrite)
        else do
          handle <- fdToHandle w
          writeIORef writeOwner (hClose handle)
          hSetBinaryMode handle True
          hSetBuffering handle NoBuffering
          pure (Output (BS.hPut handle . toBytes) closeWrite)
      pure (output, input)

readChunk :: (BS.ByteString -> a) -> Handle -> IO (Maybe a)
readChunk fromBytes handle = do
  bytes <- BS.hGetSome handle 32768
  pure (if BS.null bytes then Nothing else Just (fromBytes bytes))

closeInput :: Input a -> IO ()
closeInput (RawInput _ close) = close
closeInput (Input _ close) = close

closeOutput :: Output a -> IO ()
closeOutput (RawOutput _ close) = close
closeOutput (Output _ close) = close

worker :: Runtime -> Stage a -> Input a -> Output a -> IO ()
worker runtime stage (Input receive closeIn) (Output emit closeOut) =
  (work `catches`
    [ Handler (\PipeClosed -> pure ())
    , Handler (\err -> if isResourceVanishedError err then pure () else throwIO (err :: IOException))
    ]) `finally` (closeIn >> closeOut)
 where
  work = case stage of
    SourceStage action -> action runtime >>= emit
    SourceListStage values -> mapM_ emit values
    TransformStage transform -> transform receive emit
    MapStage f -> loop f
    External _ _ -> error "worker: external stage"
  loop f = receive >>= maybe (pure ()) (\value -> emit (f value) >> loop f)
worker _ _ _ _ = error "worker: raw endpoint"

-- Polling keeps wait cancellable when a worker fails.
--
-- A job is stopped when every live child has stopped, as after Ctrl-Z. Then
-- the park action hands the terminal back and blocks until the job should
-- continue. A child that stopped for reading or writing the terminal before
-- it owned it is continued at once, as in the spike.
--
-- In the background, reading or writing the terminal is a real stop, and the
-- job waits in the table for fg.
waitStages :: IO Bool -> IO () -> IORef [ProcessID] -> ProcessGroupID -> IO (Map.Map ProcessID Status)
waitStages isForeground park children group = go Map.empty []
 where
  go statuses stopped = do
    live <- readIORef children
    if null live then pure statuses
    else if all (`elem` stopped) live then do
      park
      go statuses []
    else do
      status <- mask_ $ do
        next <- getGroupProcessStatus False True group
        case next of
          Just (_, Stopped _) -> pure ()
          Just (pid, _) -> modifyIORef' children (filter (/= pid))
          Nothing -> pure ()
        pure next
      case status of
        Nothing -> threadDelay 5000 >> go statuses stopped
        Just (pid, Stopped sig)
          | sig == sigTTIN || sig == sigTTOU -> do
              foreground <- isForeground
              if foreground
                then quietly (signalProcessGroup sigCONT group) >> go statuses stopped
                else go statuses (pid : stopped)
        Just (pid, Stopped _) -> go statuses (pid : stopped)
        Just (pid, outcome) -> do
          let result = case outcome of
                Exited ExitSuccess -> ExitedWith 0
                Exited (ExitFailure n) -> ExitedWith n
                Terminated sig _ -> KilledBy (fromIntegral sig)
          go (Map.insert pid result statuses) (filter (/= pid) stopped)

-- | Give a job the terminal, and take it back on every path. The second
-- function is for a stopped job: it hands the terminal back to the shell, runs
-- the wait, then returns the terminal to the job with the job's own modes,
-- because a stopped editor has its own raw mode.
--
-- Only a foreground job touches the terminal. A background job that ends
-- leaves it alone, because the prompt owns it then.
withTerminal :: IO Bool -> ((Maybe ProcessGroupID -> IO ()) -> (ProcessGroupID -> IO Bool -> IO ()) -> IO a) -> IO a
withTerminal isForeground action = do
  tty <- queryTerminal stdInput
  if not tty then action (const (pure ())) (\_ wait -> void wait) else
    bracket acquire restore $ \saved ->
      action give (suspend saved)
 where
  give job = do
    foreground <- isForeground
    when foreground (forM_ job (quietly . setTerminalProcessGroupID stdInput))
  suspend :: (ProcessGroupID, TerminalAttributes, System.Posix.Signals.Handler) -> ProcessGroupID -> IO Bool -> IO ()
  suspend (group, attrs, _) job wait = do
    owned <- isForeground
    jobAttrs <- if not owned then pure attrs else do
      current <- getTerminalAttributes stdInput
      quietly (setTerminalProcessGroupID stdInput group)
      setTerminalAttributes stdInput attrs Immediately
      pure current
    foreground <- wait
    when foreground $ do
      setTerminalAttributes stdInput jobAttrs Immediately
      quietly (setTerminalProcessGroupID stdInput job)
  acquire = do
    attrs <- getTerminalAttributes stdInput
    group <- getProcessGroupID
    previous <- installHandler sigTTOU Ignore Nothing
    pure (group, attrs, previous)
  restore (group, attrs, previous) = do
    foreground <- isForeground
    (when foreground (setTerminalProcessGroupID stdInput group >> setTerminalAttributes stdInput attrs Immediately))
      `finally` void (installHandler sigTTOU previous Nothing)

-- | Run a plan once. The final external's status is the return value. Output
-- items are streamed to the sink; they are never accumulated or decoded here.
--
-- Every external in one plan sees the environment as it was when the plan
-- started, so a source that changes the session cannot split a pipeline.
runStages :: Runtime -> (BS.ByteString -> a) -> (a -> BS.ByteString) -> (a -> IO ())
          -> [Stage a] -> IO Status
runStages runtime fromBytes toBytes sink stages = do
  statuses <- runStagesAll runtime fromBytes toBytes sink stages
  pure (case reverse statuses of
    status : _ -> status
    [] -> ExitedWith 0)

-- | The same, with the status of every external, in pipeline order. A plan
-- with no externals has none.
runStagesAll :: Runtime -> (BS.ByteString -> a) -> (a -> BS.ByteString) -> (a -> IO ())
             -> [Stage a] -> IO [Status]
runStagesAll _ _ _ _ [] = pure []
runStagesAll runtime fromBytes toBytes sink stages@(firstStage : restStages) = do
  hFlush stdout
  env <- runtimeEnviron runtime
  -- This also sees commands hidden in bound stages. Resolve the entire plan
  -- before the first spawn, so a missing downstream program runs nothing.
  missing <- firstMissing env [name | External name _ <- stages]
  case missing of
    Just name -> throwIO (StreamError ("command not found: " <> name))
    Nothing -> do
      outcome <- try $ do
        when (any (any (elem '\0')) [name : args | External name args <- stages]) $
          ioError (userError "a command or argument contains NUL")
        execute env
      case outcome of
        Left err -> throwIO (StreamError ("pipeline failed: " <> show (err :: IOException)))
        Right status -> hFlush stdout >> pure status
 where
  firstMissing :: Environ -> [String] -> IO (Maybe String)
  firstMissing _ [] = pure Nothing
  firstMissing env (name : rest) = do
    found <- programExists env name
    if found then firstMissing env rest else pure (Just name)
  isForeground = maybe (pure True) hookForeground (runtimeJob runtime)
  execute env = withTerminal isForeground $ \giveTerminal suspendTerminal -> withChildren $ \(children, groupRef) -> withResources $ \resources -> do
    edges <- sequence [edge resources fromBytes toBytes l r | (l, r) <- zip stages (drop 1 stages)]
    let lastStage = foldl (\_ next -> next) firstStage restStages
        start = if isExternal firstStage
          then RawInput stdInput (pure ())
          else Input (readChunk fromBytes stdin) (pure ())
        end = if isExternal lastStage
          then RawOutput stdOutput (pure ())
          else Output sink (pure ())
        inputs = start : map snd edges
        outputs = map fst edges <> [end]
        nodes = zip3 stages inputs outputs
    pids <- fmap concat $ forM nodes $ \(stage, input, output) -> case (stage, input, output) of
      (External name args, RawInput inputFd _, RawOutput outputFd _) -> mask_ $ do
        group <- readIORef groupRef
        pid <- spawnStage env name args inputFd outputFd (maybe 0 id group)
        modifyIORef' children (pid :)
        when (group == Nothing) (writeIORef groupRef (Just pid))
        pure [pid]
      _ -> pure []
    -- Close the parent's copies before waiting or a reader can never see EOF.
    forM_ nodes $ \(stage, input, output) -> when (isExternal stage) (closeInput input >> closeOutput output)
    group <- readIORef groupRef
    forM_ group (\job -> forM_ (runtimeJob runtime) (\hooks -> hookStarted hooks job))
    giveTerminal group
    forM_ group (quietly . signalProcessGroup sigCONT)
    let workers = [worker runtime stage input output | (stage, input, output) <- nodes, not (isExternal stage)]
        -- Without a way to resume it later, a stopped job continues at once.
        park job = case runtimeJob runtime of
          Nothing -> quietly (signalProcessGroup sigCONT job)
          Just hooks -> do
            suspendTerminal job (hookPark hooks)
            quietly (signalProcessGroup sigCONT job)
        wait = maybe (pure Map.empty) (\job -> waitStages isForeground (park job) children job) group
    (statuses, _) <- concurrently wait (mapConcurrently_ id workers)
    pure [Map.findWithDefault (ExitedWith 127) pid statuses | pid <- pids]
