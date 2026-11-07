-- | Process setup and spawn primitives. 'Lambdash.Stream' owns the lifetime
-- of a foreground job, including a job with only one external stage.
module Lambdash.Process
  ( Status (..)
  , setupTerminal
  , runAsScript
  , isScript
  , currentDirectory
  , directoryGone
  , goneNotice
  , saveShellTerminal
  , shellModes
  , traceTerminal
  , giveTerminal
  , readModes
  , writeModes
  , continueGroup
  , programExists
  , resolveProgram
  , spawnStage
  ) where

import Control.Concurrent (isCurrentThreadBound, myThreadId)
import Control.Exception (IOException, try)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (unless, void, when)
import Data.Map.Strict qualified as Map
import Foreign hiding (void)
import Foreign.C
import Lambdash.Overlay (Environ)
import System.Directory (doesFileExist, findExecutablesInDirectories, getCurrentDirectory, getHomeDirectory)
import System.IO.Error (isDoesNotExistError)
import System.Environment (lookupEnv)
import Control.Exception (throwIO)
import System.Posix.IO (stdInput)
import System.Posix.Files (fileAccess)
import System.Posix.Process
import System.Posix.Signals
import System.Posix.Terminal
import System.Posix.Types
import System.IO (hPutStrLn, stderr)

foreign import ccall safe "lsh_spawn_env"
  c_spawn :: CString -> Ptr CString -> Ptr CString -> Fd -> Fd -> ProcessGroupID -> Ptr ProcessID -> IO CInt

-- | How a program ended.
data Status
  = ExitedWith Int
  | KilledBy Int
  deriving (Eq, Show)

-- | Whether this process runs a script rather than a prompt. Signal
-- handling is process-wide, so this is too: @main@ sets it once, before
-- anything runs.
-- | The last working directory that existed when λsh asked for it.
knownDirectory :: IORef (Maybe FilePath)
knownDirectory = unsafePerformIO (newIORef Nothing)
{-# NOINLINE knownDirectory #-}

-- | The working directory. When something has removed it from under the
-- shell, as when a worktree goes, it is the last one that existed: the
-- shell's logical directory, as PWD is in other shells. So the prompt
-- still shows where it was, and cd .. still leaves.
currentDirectory :: IO FilePath
currentDirectory = do
  found <- try getCurrentDirectory
  case found of
    Right directory -> writeIORef knownDirectory (Just directory) >> pure directory
    Left err
      | isDoesNotExistError err -> do
          known <- readIORef knownDirectory
          case known of
            Just directory -> pure directory
            Nothing -> maybe "/" id <$> lookupEnv "PWD"
      | otherwise -> throwIO (err :: IOException)

-- | Whether the working directory no longer exists.
directoryGone :: IO Bool
directoryGone = either (const True) (const False) <$> (try getCurrentDirectory :: IO (Either IOException FilePath))

-- | The directory whose removal the prompt last reported.
reportedGone :: IORef (Maybe FilePath)
reportedGone = unsafePerformIO (newIORef Nothing)
{-# NOINLINE reportedGone #-}

-- | Once for each directory: a note that it no longer exists.
goneNotice :: IO (Maybe String)
goneNotice = do
  gone <- directoryGone
  if not gone
    then pure Nothing
    else do
      directory <- currentDirectory
      reported <- readIORef reportedGone
      if reported == Just directory
        then pure Nothing
        else do
          writeIORef reportedGone (Just directory)
          pure (Just ("this directory no longer exists: " <> directory <> "\ncd to leave it, as cd .."))

scriptMode :: IORef Bool
scriptMode = unsafePerformIO (newIORef False)
{-# NOINLINE scriptMode #-}

-- | Run as a script: the process keeps the signals it started with, and a
-- foreground job's Ctrl-C or Ctrl-Z reaches the script too, as in a bash
-- script.
runAsScript :: IO ()
runAsScript = writeIORef scriptMode True

isScript :: IO Bool
isScript = readIORef scriptMode

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

-- | Remember the shell's own terminal modes, from before the line editor, and
-- give back an action that makes the shell own the terminal with them again.
-- The REPL runs it after every foreground wait, as bash does, so a job that
-- left the terminal in a strange state, or a handover that failed, cannot
-- outlive its line.
saveShellTerminal :: IO (IO ())
saveShellTerminal = do
  tty <- queryTerminal stdInput
  if not tty then pure (pure ()) else do
    modes <- readModes "save shell modes"
    writeIORef savedModes modes
    group <- getProcessGroupID
    pure $ do
      giveTerminal "shell takes the terminal" group
      mapM_ (writeModes "shell restores its modes") modes

-- | The shell's own modes, from before the line editor: cooked, with echo.
-- Nothing until 'saveShellTerminal' runs.
savedModes :: IORef (Maybe TerminalAttributes)
savedModes = unsafePerformIO (newIORef Nothing)
{-# NOINLINE savedModes #-}

-- | The modes a job should have when it has none of its own yet. A job that
-- starts in the background must not read the terminal's modes then: the line
-- editor may already have the terminal in raw mode for the next prompt.
shellModes :: IO (Maybe TerminalAttributes)
shellModes = readIORef savedModes

-- | With @LAMBDASH_TRACE_TTY@ set, each terminal and job-control call, its
-- result and the thread that made it go to stderr.
traceTerminal :: String -> Either IOException () -> IO ()
traceTerminal label result = do
  enabled <- lookupEnv "LAMBDASH_TRACE_TTY"
  when (maybe False (not . null) enabled) $ do
    thread <- myThreadId
    bound <- isCurrentThreadBound
    hPutStrLn stderr ("[tty] " <> show thread <> (if bound then " bound" else "") <> " " <> label
      <> ": " <> either show (const "ok") result)

traced :: String -> IO () -> IO ()
traced label action = (try action :: IO (Either IOException ())) >>= traceTerminal label

-- | tcsetpgrp. It never throws: a failure is traced, and the REPL takes the
-- terminal back after every wait anyway.
giveTerminal :: String -> ProcessGroupID -> IO ()
giveTerminal label group = traced (label <> " (tcsetpgrp " <> show group <> ")") (setTerminalProcessGroupID stdInput group)

readModes :: String -> IO (Maybe TerminalAttributes)
readModes label = do
  result <- try (getTerminalAttributes stdInput) :: IO (Either IOException TerminalAttributes)
  traceTerminal (label <> " (tcgetattr)") (() <$ result)
  pure (either (const Nothing) Just result)

writeModes :: String -> TerminalAttributes -> IO ()
writeModes label modes = traced (label <> " (tcsetattr)") (setTerminalAttributes stdInput modes Immediately)

continueGroup :: String -> ProcessGroupID -> IO ()
continueGroup label group = traced (label <> " (SIGCONT " <> show group <> ")") (signalProcessGroup sigCONT group)

-- macOS refuses kill and tcsetpgrp for a group that holds only zombies, and a
-- short command such as `true` can be one before the shell gets here.
quietly :: IO () -> IO ()
quietly = void . (try :: IO a -> IO (Either IOException a))

-- | Replace a leading @~/@ with the home directory.
expandHome :: String -> IO String
expandHome ('~' : '/' : rest) = (<> "/" <> rest) <$> getHomeDirectory
expandHome path = pure path

-- | Can this name start a program, in this environment?
programExists :: Environ -> String -> IO Bool
programExists env name = maybe False (const True) <$> resolveProgram env name

-- | The file a name starts. A name with a slash is a path, and any other name
-- is searched for on the @PATH@ of the given environment, which is the
-- effective one: an overlay that prepends to @PATH@ changes what runs.
--
-- An empty @PATH@ entry means the current directory, as in POSIX. With no
-- @PATH@ at all, search a default, as bash does.
resolveProgram :: Environ -> String -> IO (Maybe FilePath)
resolveProgram env name
  | '/' `elem` name = do
      file <- expandHome name
      result <- try (do
        exists <- doesFileExist file
        if exists then fileAccess file False False True else pure False) :: IO (Either IOException Bool)
      pure (if either (const False) id result then Just file else Nothing)
  | null name = pure Nothing
  | otherwise = do
      let path = Map.findWithDefault "/usr/bin:/bin" "PATH" env
          directories = map (\entry -> if null entry then "." else entry) (splitPath path)
      found <- try (findExecutablesInDirectories directories name) :: IO (Either IOException [FilePath])
      pure (case found of
        Right (file : _) -> Just file
        _ -> Nothing)
 where
  splitPath text = case break (== ':') text of
    (entry, []) -> [entry]
    (entry, _ : rest) -> entry : splitPath rest

-- | Spawn a stage into a shared process group, with explicit pipe endpoints and
-- an explicit environment. The program is resolved in that environment.
--
-- @argv[0]@ is the name as typed, with @~/@ expanded, as in other shells.
spawnStage :: Environ -> String -> [String] -> Fd -> Fd -> ProcessGroupID -> IO ProcessID
spawnStage env name args input output group = do
  argv0 <- expandHome name
  resolved <- resolveProgram env name
  file <- maybe (ioError (userError ("command not found: " <> name))) pure resolved
  let variables = [key <> "=" <> value | (key, value) <- Map.toList env]
  when (any (elem '\0') (file : args <> variables)) $
    ioError (userError "a command, argument or variable contains NUL")
  withMany withCString (argv0 : args) $ \cstrs ->
    withArray0 nullPtr cstrs $ \cargv ->
      withMany withCString variables $ \cvars ->
        withArray0 nullPtr cvars $ \cenvp ->
          withCString file $ \cfile ->
            alloca $ \pid -> do
              err <- c_spawn cfile cargv cenvp input output group pid
              unless (err == 0) $
                ioError (errnoToIOError "spawn" (Errno err) Nothing (Just file))
              peek pid
