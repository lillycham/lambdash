-- | What a running computation can reach. A computation receives a 'Runtime'
-- when it runs, not when it is built, so a value built earlier sees the
-- session as it is when it runs.
module Lambdash.Runtime
  ( Runtime (..)
  , newRuntime
  , newRuntimeWith
  , runtimeEnviron
  ) where

import Data.IORef
import Data.Map.Strict qualified as Map
import Lambdash.Overlay
import System.Directory (getCurrentDirectory)
import System.Environment (getEnvironment)

data Runtime = Runtime
  { -- | The session's environments. A computation that changes them changes
    -- this reference, and the REPL commits it only when the line succeeds.
    runtimeState :: IORef ShellState
  , -- | Where Nix GC roots go. No directory means that nothing is rooted.
    runtimeRoots :: Maybe FilePath
  , -- | What to do when every process of a job has stopped, as after Ctrl-Z.
    -- It blocks until the job should continue. Without it, a stopped job is
    -- continued at once, because nothing could resume it later.
    runtimePark :: Maybe (IO ())
  , -- | Layers that @with@ adds, innermost last. They are not in the session:
    -- they last for the extent of a block.
    runtimeLocal :: [Overlay]
  }

-- | A runtime with no GC roots and no job control.
newRuntime :: ShellState -> IO Runtime
newRuntime = newRuntimeWith Nothing

newRuntimeWith :: Maybe FilePath -> ShellState -> IO Runtime
newRuntimeWith roots state = do
  ref <- newIORef state
  pure (Runtime ref roots Nothing [])

-- | The effective environment now.
runtimeEnviron :: Runtime -> IO Environ
runtimeEnviron runtime = do
  base <- Map.fromList <$> getEnvironment
  state <- readIORef (runtimeState runtime)
  cwd <- getCurrentDirectory
  pure (effective base (withLayers (runtimeLocal runtime) (inScope cwd state)))
