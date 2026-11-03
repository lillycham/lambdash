-- | Resolvers that build overlays from Nix. See @docs/nix-notes.md@ for the
-- Nix behaviour this relies on, as checked against Nix 2.34.
--
-- > pkg :: String -> Comp {exec nix} Overlay              -- outputs on PATH
-- > dev :: String -> Comp {exec nix, exec bash} Overlay   -- a devshell
--
-- Both run Nix as an ordinary foreground job, so Ctrl-C stops a build and
-- Nix's progress reaches the terminal. Resolving changes nothing in the
-- session: 'Lambdash.Overlay.useOverlay' commits the result.
module Lambdash.Nix
  ( resolvePkg
  , resolveDev
  , pkgOverlay
  , diffEnviron
  , dropped
  , parseEnv0
  , markHook
  , hookMarker
  , newFunctions
  , withRootDirectory
  ) where

import Control.Exception (IOException, bracket, finally, throwIO, try)
import Control.Monad (forM_, unless, void, when)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.IORef
import Data.List (isPrefixOf, isSuffixOf, sort, (\\))
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as Text
import GHC.Clock (getMonotonicTimeNSec)
import Lambdash.Overlay
import Lambdash.Process (Status (..))
import Lambdash.Stream (Stage (..), StreamError (..), runStages)
import System.Directory
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (nullSignal, signalProcess)
import Text.Read (readMaybe)

-- | Run one program to completion, with its stdout captured and its stderr on
-- the terminal.
captureProgram :: Runtime -> String -> [String] -> IO (Status, BS.ByteString)
captureProgram runtime name args = do
  chunks <- newIORef []
  status <- runStages runtime id id (\chunk -> modifyIORef' chunks (chunk :))
    [External name args, MapStage id]
  output <- BS.concat . reverse <$> readIORef chunks
  pure (status, output)

-- | Run a program for its effect, with stdout and stderr on the terminal.
runProgram :: Runtime -> String -> [String] -> IO Status
runProgram runtime name args = runStages runtime id id BS.putStr [External name args]

failed :: String -> Status -> IO a
failed what status = throwIO (StreamError (what <> " failed: " <> describe status))
 where
  describe (ExitedWith n) = "exit " <> show n
  describe (KilledBy n) = "signal " <> show n

decode :: BS.ByteString -> String
decode = Text.unpack . Text.decodeUtf8With Text.lenientDecode

-- | A fresh name in the root directory, for one link.
rootLink :: Runtime -> String -> IO (Maybe FilePath)
rootLink runtime kind = case runtimeRoots runtime of
  Nothing -> pure Nothing
  Just directory -> do
    stamp <- getMonotonicTimeNSec
    pure (Just (directory <> "/" <> kind <> "-" <> show stamp))

-- | @pkg ref@: build the outputs that @nix shell@ would use, and put each
-- output's @bin@ on @PATH@. This is all that @nix shell@ changes.
--
-- The build is rooted for the rest of the session when the session has a
-- root directory.
resolvePkg :: Runtime -> String -> IO Overlay
resolvePkg runtime ref = do
  link <- rootLink runtime "pkg"
  let rooting = maybe ["--no-link"] (\path -> ["--out-link", path]) link
  (status, output) <- captureProgram runtime "nix" (["build", ref, "--print-out-paths"] <> rooting)
  unless (status == ExitedWith 0) (failed ("pkg " <> ref) status)
  pure (pkgOverlay ref (lines (decode output)))

-- | The overlay for some store paths, in the order Nix printed them. The first
-- path ends up first on @PATH@, as with @nix shell@. A path without a @bin@
-- directory is still added, as Nix does.
pkgOverlay :: String -> [FilePath] -> Overlay
pkgOverlay ref paths =
  Overlay ref [Patch "PATH" (Prepend ":" (path <> "/bin")) | path <- reverse (filter (not . null) paths)]

-- | Variables a devshell overlay never carries.
--
-- * The temporary directory names the @mktemp@ of one resolve, which outlives
--   it and would go stale.
-- * @SHELL@ is Nix's own bash, and other programs read it.
-- * @PWD@, @OLDPWD@, @SHLVL@ and @_@ belong to the bash that ran the hook.
-- * @out@ is @$PWD/outputs/out@ and @name@ is the env derivation's name: noise
--   that would flag a conflict between any two devshells.
-- * @shellHook@ is the hook's source. The hook has already run, and replaying
--   an overlay never runs it again.
dropped :: [String]
dropped =
  [ "TMPDIR", "TMP", "TEMP", "TEMPDIR", "NIX_BUILD_TOP"
  , "SHELL", "PWD", "OLDPWD", "SHLVL", "_"
  , "out", "name", "shellHook"
  ]

-- | The patches that turn one environment into another. An unchanged variable
-- gives nothing. A value that ends with @:old@ is a prepend, one that starts
-- with @old:@ is an append, and anything else is a set. This is the shape of
-- @PATH@ and @XDG_DATA_DIRS@ after the devshell script joins them.
--
-- Patches come in name order, so a diff is deterministic.
diffEnviron :: Environ -> Environ -> [Patch]
diffEnviron before after = changed <> removed
 where
  keep name = name `notElem` dropped
  changed =
    [ Patch name op
    | (name, value) <- Map.toAscList after
    , keep name
    , Just op <- [change (Map.lookup name before) value]
    ]
  removed = [Patch name Unset | name <- Map.keys (Map.difference before after), keep name]
  change Nothing value = Just (Set value)
  change (Just old) value
    | old == value = Nothing
    | not (null old) && (':' : old) `isSuffixOf` value =
        Just (Prepend ":" (take (length value - length old - 1) value))
    | not (null old) && (old <> ":") `isPrefixOf` value =
        Just (Append ":" (drop (length old + 1) value))
    | otherwise = Just (Set value)

-- | The output of @env -0@.
parseEnv0 :: BS.ByteString -> Environ
parseEnv0 bytes = Map.fromList
  [ (decode name, decode (BS.drop 1 value))
  | entry <- BS.split 0 bytes
  , not (BS.null entry)
  , let (name, value) = B8.break (== '=') entry
  , not (BS.null value)
  ]

-- | The line of the devshell script that runs the hook. Nix prints it last.
hookMarker :: String
hookMarker = "eval \"${shellHook:-}\""

-- | Record the functions that exist just before the hook runs, so that the
-- hook's own functions can be told apart from stdenv's. Nothing changes when
-- the line is missing, and then no functions are reported.
markHook :: FilePath -> String -> String
markHook before script = unlines (map mark (lines script))
 where
  mark line
    | line == hookMarker = "declare -F > " <> show before <> "; " <> line
    | otherwise = line

-- | Functions in the second @declare -F@ listing and not the first.
newFunctions :: String -> String -> [String]
newFunctions before after = sort (names after \\ names before)
 where
  names text = [name | line <- lines text, ["declare", "-f", name] <- [words line]]

-- | @dev ref@: the devshell, with its hook run once, as a diff against the
-- effective environment.
--
-- @nix print-dev-env@ without @--json@ prints a script that joins @PATH@ onto
-- the outer value, makes a temporary directory and runs @shellHook@ last.
-- Sourcing it in a clean bash is what @nix develop@ and direnv do. The hook
-- runs here, and never again: an overlay records what the hook set, not what
-- it did.
resolveDev :: Runtime -> String -> IO Overlay
resolveDev runtime ref = do
  link <- rootLink runtime "dev"
  let rooting = maybe [] (\path -> ["--profile", path]) link
  (status, script) <- captureProgram runtime "nix" (["print-dev-env"] <> rooting <> [ref])
  unless (status == ExitedWith 0) (failed ("dev " <> ref) status)
  outer <- runtimeEnviron runtime
  withScratch $ \directory -> do
    let file name = directory <> "/" <> name
    writeFile (file "script.sh") (markHook (file "before") (decode script))
    -- The paths are positional and the script runs with none, so a hook sees
    -- the same arguments as under nix develop.
    let body = concat
          [ "__lsh_env=$1; __lsh_after=$2; __lsh_script=$3; set --; "
          , "source \"$__lsh_script\"; "
          , "env -0 > \"$__lsh_env\"; declare -F > \"$__lsh_after\""
          ]
    hookStatus <- runProgram runtime "bash"
      ["--norc", "--noprofile", "-c", body, "lambdash-dev", file "env", file "after", file "script.sh"]
    unless (hookStatus == ExitedWith 0) (failed ("dev " <> ref <> ": the devshell script") hookStatus)
    inner <- parseEnv0 <$> BS.readFile (file "env")
    functions <- do
      hasBefore <- doesFileExist (file "before")
      if not hasBefore then pure [] else
        newFunctions <$> readFile' (file "before") <*> readFile' (file "after")
    unless (null functions) $
      hPutStrLn stderr ("dev " <> ref <> ": the hook defined " <> count functions
        <> ", not carried over: " <> unwords functions)
    pure (Overlay ref (diffEnviron outer inner))
 where
  count [_] = "1 function"
  count names = show (length names) <> " functions"
  readFile' path = do
    text <- readFile path
    length text `seq` pure text

-- | A temporary directory for one resolve, removed afterwards.
withScratch :: (FilePath -> IO a) -> IO a
withScratch action = do
  base <- getTemporaryDirectory
  stamp <- getMonotonicTimeNSec
  let directory = base <> "/lambdash-dev-" <> show stamp
  bracket (createDirectory directory >> pure directory) remove action
 where
  remove directory = void (try (removeDirectoryRecursive directory) :: IO (Either IOException ()))

-- | Give the session a directory for GC roots, and remove it when the session
-- ends. A root is a symlink there, which Nix records as an indirect root, and
-- deleting the symlink drops the root.
--
-- Roots last for the session, not only while an overlay is active: an
-- overlay is a value, and @use e@ can activate it again after @unuse@. The
-- directories of sessions that died without cleaning up are removed here.
--
-- Without a usable directory, the session roots nothing.
withRootDirectory :: (Maybe FilePath -> IO a) -> IO a
withRootDirectory action = do
  state <- lookupEnv "XDG_STATE_HOME"
  home <- getHomeDirectory
  pid <- getProcessID
  let base = maybe (home <> "/.local/state") id (nonEmpty state) <> "/lambdash/roots"
      mine = base <> "/" <> show pid
  made <- try (do
    createDirectoryIfMissing True base
    removeStale base
    createDirectoryIfMissing False mine) :: IO (Either IOException ())
  case made of
    Left _ -> action Nothing
    Right () -> action (Just mine) `finally` remove mine
 where
  nonEmpty (Just value) | not (null value) = Just value
  nonEmpty _ = Nothing
  remove directory = void (try (removeDirectoryRecursive directory) :: IO (Either IOException ()))
  removeStale base = do
    entries <- listDirectory base
    forM_ entries $ \entry -> case readMaybe entry of
      Nothing -> pure ()
      Just pid -> do
        alive <- try (signalProcess nullSignal pid) :: IO (Either IOException ())
        when (either (const True) (const False) alive) (remove (base <> "/" <> entry))
