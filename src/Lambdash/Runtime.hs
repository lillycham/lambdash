-- | What a running computation can reach. A computation receives a 'Runtime'
-- when it runs, not when it is built, so a value built earlier sees the
-- session as it is when it runs.
module Lambdash.Runtime
  ( Runtime (..)
  , JobHooks (..)
  , Job (..)
  , JobTable (..)
  , JobState (..)
  , startJob
  , continueJob
  , awaitDone
  , jobState
  , markCollected
  , newRuntime
  , newRuntimeWith
  , runtimeEnviron
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (forM_, when)
import Data.IORef
import Lambdash.Process (continueGroup, giveTerminal)
import System.Posix.IO (stdInput)
import System.Posix.Terminal (queryTerminal)
import Data.Map.Strict qualified as Map
import Lambdash.Overlay
import Lambdash.Process (currentDirectory)
import System.Environment (getEnvironment)
import System.Posix.Types (ProcessGroupID)

data Runtime = Runtime
  { -- | The session's environments. A computation that changes them changes
    -- this reference, and the REPL commits it only when the line succeeds.
    runtimeState :: IORef ShellState
  , -- | Where Nix GC roots go. No directory means that nothing is rooted.
    runtimeRoots :: Maybe FilePath
  , -- | How a job reaches the job table. Without it, a stopped job is
    -- continued at once, because nothing could resume it later.
    runtimeJob :: Maybe JobHooks
  , -- | Layers that @with@ adds, innermost last. They are not in the session:
    -- they last for the extent of a block.
    runtimeLocal :: [Overlay]
  , -- | The REPL line this runs for, to name the jobs it starts.
    runtimeLine :: String
  , -- | Where @&@ registers the jobs it starts, for the REPL's table.
    runtimeTable :: Maybe JobTable
  }

-- | A computation running on its own thread, with the processes it starts. A
-- line stopped by Ctrl-Z and a job started by @&@ are both jobs.
--
-- The result is filled once and can be read any number of times, so a job
-- handle can be waited on more than once. Whether it is stopped is separate
-- state, because a stopped job has no result yet.
data Job = Job
  { jobNumber :: Int
  , jobLine :: String
  , -- | Every process has stopped, and the thread is parked until resumed.
    jobStopped :: TVar Bool
  , -- | Wakes a parked job: True in the foreground, False in the background.
    jobResume :: TMVar Bool
  , -- | Whether the job owns the terminal. The job's thread reads it too.
    jobOwned :: IORef Bool
  , -- | The process group of the job's current pipeline.
    jobGroup :: IORef (Maybe ProcessGroupID)
  , -- | How it ended, for notices: an error, or the result as text.
    jobOutcome :: TMVar (Either String String)
  , -- | Something took the result with @wait@ or @fg@, so no notice is due.
    jobCollected :: TVar Bool
  }

-- | Jobs that @&@ started during one line, and the next job number.
data JobTable = JobTable
  { tableNext :: IORef Int
  , tableNew :: IORef [Job]
  }

data JobState = Running | Stopped | Done
  deriving (Eq, Show)

-- | Start a job on its own thread, owning the terminal or not. The typed
-- result goes to the returned variable, and a summary to 'jobOutcome'.
startJob :: Runtime -> Int -> String -> Bool -> (Runtime -> IO v) -> (Either SomeException v -> Either String String)
         -> IO (Job, TMVar (Either SomeException v))
startJob runtime number line owns action summarise = do
  stopped <- newTVarIO False
  resume <- newEmptyTMVarIO
  owned <- newIORef owns
  group <- newIORef Nothing
  outcome <- newEmptyTMVarIO
  collected <- newTVarIO False
  result <- newEmptyTMVarIO
  let job = Job number line stopped resume owned group outcome collected
      hooks = JobHooks
        { hookPark = do
            atomically (writeTVar stopped True)
            next <- atomically (takeTMVar resume)
            writeIORef owned next
            pure next
        , hookStarted = writeIORef group . Just
        , hookForeground = readIORef owned
        }
  _ <- forkIO $ do
    value <- try (action runtime {runtimeJob = Just hooks})
    atomically (putTMVar result value >> putTMVar outcome (summarise value))
  pure (job, result)

-- | Let a job go on, in the foreground or the background. A parked job is
-- woken. A running job brought to the foreground is handed the terminal here,
-- because it is not parked to take it itself.
continueJob :: Bool -> Job -> IO ()
continueJob foreground job = do
  wasStopped <- atomically $ do
    parked <- readTVar (jobStopped job)
    when parked $ do
      writeTVar (jobStopped job) False
      putTMVar (jobResume job) foreground
    pure parked
  when (not wasStopped && foreground) $ do
    writeIORef (jobOwned job) True
    group <- readIORef (jobGroup job)
    tty <- queryTerminal stdInput
    forM_ group $ \pgid -> do
      when tty (giveTerminal "a running job takes the terminal" pgid)
      continueGroup "continue a running job" pgid

-- | Wait until the job is done, giving the result, or stopped, giving nothing.
awaitDone :: Job -> STM a -> IO (Maybe a)
awaitDone job done = atomically $
  (Just <$> done) `orElse` (readTVar (jobStopped job) >>= check >> pure Nothing)

jobState :: Job -> STM JobState
jobState job = do
  finished <- not <$> isEmptyTMVar (jobOutcome job)
  parked <- readTVar (jobStopped job)
  pure (if finished then Done else if parked then Stopped else Running)

markCollected :: Job -> IO ()
markCollected job = atomically (writeTVar (jobCollected job) True)

-- | How a line's processes talk to the REPL's job table.
data JobHooks = JobHooks
  { -- | Every process of the job has stopped. Block until it should go on,
    -- and say whether it goes on in the foreground.
    hookPark :: IO Bool
  , -- | A pipeline of the job has its process group. A block can run several.
    hookStarted :: ProcessGroupID -> IO ()
  , -- | Whether the job owns the terminal now. A background job must not
    -- take it, or restore its modes over the prompt.
    hookForeground :: IO Bool
  }

-- | A runtime with no GC roots and no job control.
newRuntime :: ShellState -> IO Runtime
newRuntime = newRuntimeWith Nothing

newRuntimeWith :: Maybe FilePath -> ShellState -> IO Runtime
newRuntimeWith roots state = do
  ref <- newIORef state
  pure (Runtime ref roots Nothing [] "" Nothing)

-- | The effective environment now.
runtimeEnviron :: Runtime -> IO Environ
runtimeEnviron runtime = do
  base <- Map.fromList <$> getEnvironment
  state <- readIORef (runtimeState runtime)
  cwd <- currentDirectory
  pure (effective base (withLayers (runtimeLocal runtime) (inScope cwd state)))
