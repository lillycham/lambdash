{-# LANGUAGE OverloadedStrings #-}

-- | Approval for activating a directory's flake when the shell enters it.
--
-- Typing @dev .#@ is consent, as typing @nix develop@ is. Entering a directory
-- is not: a cloned repository's @shellHook@ would run. So a directory's devshell
-- activates on @cd@ only after @allow@.
--
-- The approval names the hash of the flake's source as Nix sees it: every
-- file of a path flake, or every tracked file of a git flake. The devshell is
-- then evaluated from the copy in the store that has that hash, so the check
-- and the evaluation read the same files. A file that a hook reads from the
-- working directory when it runs is outside the approval.
--
-- A quick fingerprint of @flake.nix@ and @flake.lock@ notices a changed flake
-- on each arrival without asking Nix to copy the source.
module Lambdash.Trust
  ( flakeHash
  , Source (..)
  , flakeSource
  , parseSource
  , autoName
  , allowedFile
  , approved
  , allow
  , deny
  , parseAllowed
  , renderAllowed
  ) where

import Control.Exception (IOException, bracket, evaluate, try)
import Data.Aeson (FromJSON (..), eitherDecodeStrict, withObject, (.:))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import System.Directory
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (splitDirectories, takeDirectory, (</>))
import System.IO (SeekMode (..))
import System.Posix.IO (LockRequest (..), OpenFileFlags (..), OpenMode (..), closeFd, defaultFileFlags, openFd, waitToSetLock)
import System.Posix.Types (Fd)
import System.Process (readProcessWithExitCode)

-- | A fingerprint of the directory's @flake.nix@ and @flake.lock@, or
-- nothing when there is no @flake.nix@ or Nix cannot hash it. A missing lock
-- hashes as absent, so creating one is a change.
flakeHash :: FilePath -> IO (Maybe String)
flakeHash dir = do
  hasFlake <- doesFileExist (dir <> "/flake.nix")
  if not hasFlake then pure Nothing else do
    hasLock <- doesFileExist (dir <> "/flake.lock")
    let files = [dir <> "/flake.nix"] <> [dir <> "/flake.lock" | hasLock]
    result <- try (readProcessWithExitCode "nix" (["hash", "file", "--type", "sha256", "--base16"] <> files) "")
      :: IO (Either IOException (ExitCode, String, String))
    pure $ case result of
      Right (ExitSuccess, out, _) -> Just (concat (lines out) <> (if hasLock then "" else "-nolock"))
      _ -> Nothing

-- | A flake's source as Nix sees it: the hash of the tree, and its copy in
-- the store.
data Source = Source
  { sourceHash :: String
  , sourcePath :: FilePath
  }
  deriving (Eq, Show)

instance FromJSON Source where
  parseJSON = withObject "flake metadata" $ \o -> do
    locked <- o .: "locked"
    Source <$> withObject "locked" (.: "narHash") locked <*> o .: "path"

-- | The source of the flake in a directory, or what went wrong. Nix copies
-- the tree to the store, and does not write a lock file.
flakeSource :: FilePath -> IO (Either String Source)
flakeSource dir = do
  result <- try (readProcessWithExitCode "nix" ["flake", "metadata", "--json", "--no-write-lock-file", dir] "")
    :: IO (Either IOException (ExitCode, String, String))
  pure $ case result of
    Left err -> Left (show err)
    Right (ExitSuccess, out, _) -> parseSource out
    Right (_, _, err) -> Left (concat (take 1 (filter (not . null) (reverse (lines err)))))

-- | What @nix flake metadata --json@ says, as a source.
parseSource :: String -> Either String Source
parseSource = eitherDecodeStrict . Text.encodeUtf8 . Text.pack

-- | The name of a directory's automatic overlay: its last component and a
-- @#@, as in @akyuu#@. When another directory has that name in @taken@, more
-- of the path goes in, as in @work/akyuu#@, so two directories never share
-- an overlay.
autoName :: Map String FilePath -> FilePath -> String
autoName taken dir = case [name | name <- candidates, maybe True (== dir) (Map.lookup name taken)] of
  name : _ -> name
  [] -> dir <> "#"
 where
  parts = filter (/= "/") (splitDirectories dir)
  candidates
    | null parts = ["/#"]
    | otherwise = [foldr1 (\a b -> a <> "/" <> b) (drop n parts) <> "#" | n <- reverse [0 .. length parts - 1]]

allowedFile :: IO FilePath
allowedFile = do
  state <- lookupEnv "XDG_STATE_HOME"
  home <- getHomeDirectory
  let base = case state of
        Just value | not (null value) -> value
        _ -> home <> "/.local/state"
  pure (base </> "lambdash" </> "allowed")

-- | One line per directory: the hash, a space, the directory.
parseAllowed :: String -> Map FilePath String
parseAllowed text = Map.fromList
  [ (dir, hash)
  | line <- lines text
  , let (hash, rest) = break (== ' ') line
  , not (null hash)
  , ' ' : dir <- [rest]
  ]

renderAllowed :: Map FilePath String -> String
renderAllowed entries = unlines [hash <> " " <> dir | (dir, hash) <- sortOn fst (Map.toList entries)]

readAllowed :: IO (Map FilePath String)
readAllowed = do
  file <- allowedFile
  -- Read it all before returning: an update writes the same file next.
  result <- try (readFile file >>= \text -> evaluate (length text) >> pure text) :: IO (Either IOException String)
  pure $ case result of
    Left _ -> Map.empty
    Right text -> parseAllowed text

-- | Change the approvals. Shells can run @allow@ at the same time, so each
-- update holds a lock while it reads and writes, and the new file replaces
-- the old one whole, by rename.
update :: (Map FilePath String -> Map FilePath String) -> IO ()
update change = do
  file <- allowedFile
  createDirectoryIfMissing True (takeDirectory file)
  withLock (file <> ".lock") $ do
    entries <- readAllowed
    let temporary = file <> ".new"
    writeFile temporary (renderAllowed (change entries))
    renameFile temporary file

withLock :: FilePath -> IO a -> IO a
withLock path action = bracket open closeFd (\fd -> waitToSetLock fd (WriteLock, AbsoluteSeek, 0, 0) >> action)
 where
  open :: IO Fd
  open = openFd path WriteOnly defaultFileFlags {creat = Just 0o600}

-- | The source hash approved for a directory, if any.
approved :: FilePath -> IO (Maybe String)
approved dir = Map.lookup dir <$> readAllowed

allow :: FilePath -> String -> IO ()
allow dir hash = update (Map.insert dir hash)

deny :: FilePath -> IO ()
deny dir = update (Map.delete dir)
