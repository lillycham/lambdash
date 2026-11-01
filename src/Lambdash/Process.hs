-- | Run external programs. This holds what the job-control spike proved: a child
-- gets its own process group and the terminal, and the shell takes both back.
--
-- There is no job table yet, so a stopped program gets a SIGCONT at once. Ctrl-Z
-- does nothing, and no program is left behind with nothing to resume it.
module Lambdash.Process
  ( Status (..)
  , setupTerminal
  , programExists
  , runExternal
  ) where

import Control.Exception (IOException, try)
import Control.Monad (unless, void, when)
import Foreign hiding (void)
import Foreign.C
import System.Directory (doesFileExist, findExecutable, getHomeDirectory)
import System.Exit (ExitCode (..))
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.Posix.IO (stdInput, stdOutput)
import System.Posix.Process
import System.Posix.Signals
import System.Posix.Terminal
import System.Posix.Types

foreign import ccall safe "lsh_spawn"
  c_spawn :: CString -> Ptr CString -> Fd -> Fd -> ProcessGroupID -> Ptr ProcessID -> IO CInt

-- | How a program ended.
data Status
  = ExitedWith Int
  | KilledBy Int
  deriving (Eq, Show)

-- | Make this process able to own the terminal. Call it once, at the start of an
-- interactive session. It does nothing without a terminal.
setupTerminal :: IO ()
setupTerminal = do
  tty <- queryTerminal stdInput
  when tty $ do
    mapM_
      (\sig -> installHandler sig Ignore Nothing)
      [sigINT, sigQUIT, sigTSTP, sigTTIN, sigTTOU]
    -- A session leader already leads its group.
    quietly (setProcessGroupIDOf 0 0)
    group <- getProcessGroupID
    quietly (setTerminalProcessGroupID stdInput group)

-- macOS refuses kill and tcsetpgrp for a group that holds only zombies, and a
-- short command such as `true` can be one before the shell gets here.
quietly :: IO () -> IO ()
quietly = void . (try :: IO a -> IO (Either IOException a))

-- | Replace a leading @~/@ with the home directory.
expandHome :: String -> IO String
expandHome ('~' : '/' : rest) = (<> "/" <> rest) <$> getHomeDirectory
expandHome path = pure path

-- | Can this name start a program? A name with a slash is a path, and any other
-- name is searched for on @PATH@.
programExists :: String -> IO Bool
programExists name
  | '/' `elem` name = expandHome name >>= doesFileExist
  | otherwise = maybe False (const True) <$> findExecutable name

spawn :: String -> [String] -> IO ProcessID
spawn file args =
  withMany withCString (file : args) $ \cstrs ->
    withArray0 nullPtr cstrs $ \cargv ->
      withCString file $ \cfile ->
        alloca $ \pid -> do
          err <- c_spawn cfile cargv stdInput stdOutput 0 pid
          unless (err == 0) $
            ioError (errnoToIOError "spawn" (Errno err) Nothing (Just file))
          peek pid

-- | Wait for a program to end. See the module header for stopped programs.
wait :: ProcessID -> IO Status
wait pid =
  getProcessStatus True True pid >>= \status -> case status of
    Nothing -> wait pid
    Just (Stopped _) -> do
      quietly (signalProcessGroup sigCONT pid)
      wait pid
    Just (Exited ExitSuccess) -> pure (ExitedWith 0)
    Just (Exited (ExitFailure n)) -> pure (ExitedWith n)
    Just (Terminated sig _) -> pure (KilledBy (fromIntegral sig))

-- | Give the program the terminal, wait, and take the terminal back. A program
-- can leave the terminal in any mode, so the shell restores its own.
foreground :: ProcessID -> IO Status
foreground pid = do
  attrs <- getTerminalAttributes stdInput
  group <- getProcessGroupID
  -- Only the foreground group can call tcsetpgrp without this.
  previous <- installHandler sigTTOU Ignore Nothing
  quietly (setTerminalProcessGroupID stdInput pid)
  quietly (signalProcessGroup sigCONT pid)
  status <- wait pid
  setTerminalProcessGroupID stdInput group
  setTerminalAttributes stdInput attrs Immediately
  void (installHandler sigTTOU previous Nothing)
  pure status

-- | Run a program to its end. It inherits stdin, stdout, stderr and the
-- environment. A program that cannot start gets the usual status 127.
runExternal :: String -> [String] -> IO Status
runExternal name args = do
  hFlush stdout
  file <- expandHome name
  started <- try (spawn file args) :: IO (Either IOException ProcessID)
  case started of
    Left _ -> do
      hPutStrLn stderr ("command not found: " <> name)
      pure (ExitedWith 127)
    Right pid -> do
      tty <- queryTerminal stdInput
      if tty then foreground pid else wait pid
