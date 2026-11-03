-- | Process setup and spawn primitives. 'Lambdash.Stream' owns the lifetime
-- of a foreground job, including a job with only one external stage.
module Lambdash.Process
  ( Status (..)
  , setupTerminal
  , programExists
  , resolveProgram
  , spawnStage
  ) where

import Control.Exception (IOException, try)
import Control.Monad (unless, void, when)
import Data.Map.Strict qualified as Map
import Foreign hiding (void)
import Foreign.C
import Lambdash.Overlay (Environ)
import System.Directory (doesFileExist, findExecutablesInDirectories, getHomeDirectory)
import System.Posix.IO (stdInput)
import System.Posix.Files (fileAccess)
import System.Posix.Process
import System.Posix.Signals
import System.Posix.Terminal
import System.Posix.Types

foreign import ccall safe "lsh_spawn_env"
  c_spawn :: CString -> Ptr CString -> Ptr CString -> Fd -> Fd -> ProcessGroupID -> Ptr ProcessID -> IO CInt

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
