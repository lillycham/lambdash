-- | Process setup and spawn primitives. 'Lambdash.Stream' owns the lifetime
-- of a foreground job, including a job with only one external stage.
module Lambdash.Process
  ( Status (..)
  , setupTerminal
  , programExists
  , spawnStage
  ) where

import Control.Exception (IOException, try)
import Control.Monad (unless, void, when)
import Foreign hiding (void)
import Foreign.C
import System.Directory (doesFileExist, findExecutable, getHomeDirectory)
import System.Posix.IO (stdInput)
import System.Posix.Files (fileAccess)
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
  | '/' `elem` name = do
      file <- expandHome name
      result <- try (do
        exists <- doesFileExist file
        if exists then fileAccess file False False True else pure False) :: IO (Either IOException Bool)
      pure (either (const False) id result)
  | otherwise = maybe False (const True) <$> findExecutable name

-- | Spawn a stage into a shared process group, with explicit pipe endpoints.
spawnStage :: String -> [String] -> Fd -> Fd -> ProcessGroupID -> IO ProcessID
spawnStage name args input output group = do
  file <- expandHome name
  when (any (elem '\0') (file : args)) $
    ioError (userError "a command or argument contains NUL")
  withMany withCString (file : args) $ \cstrs ->
    withArray0 nullPtr cstrs $ \cargv ->
      withCString file $ \cfile ->
        alloca $ \pid -> do
          err <- c_spawn cfile cargv input output group pid
          unless (err == 0) $
            ioError (errnoToIOError "spawn" (Errno err) Nothing (Just file))
          peek pid
