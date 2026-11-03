-- | Approval for activating a directory's flake when the shell enters it.
--
-- Typing @dev .#@ is consent, as typing @nix develop@ is. Entering a directory
-- is not: a cloned repository's @shellHook@ would run. So a directory's devshell
-- activates on @cd@ only after @allow@, and the approval names a hash of
-- @flake.nix@ and @flake.lock@. When either changes, the shell asks again.
module Lambdash.Trust
  ( flakeHash
  , autoName
  , allowedFile
  , isAllowed
  , allow
  , deny
  , parseAllowed
  , renderAllowed
  ) where

import Control.Exception (IOException, evaluate, try)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import System.Directory
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

-- | A hash of the directory's @flake.nix@ and @flake.lock@, or nothing when
-- there is no @flake.nix@. A missing lock hashes as absent, so creating one
-- asks again.
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

-- | The name of a directory's automatic overlay: its last component and a
-- @#@, as in @akyuu#@.
autoName :: FilePath -> String
autoName dir = case reverse (takeWhile (/= '/') (reverse dir)) of
  "" -> "/#"
  base -> base <> "#"

allowedFile :: IO FilePath
allowedFile = do
  state <- lookupEnv "XDG_STATE_HOME"
  home <- getHomeDirectory
  let base = case state of
        Just value | not (null value) -> value
        _ -> home <> "/.local/state"
  pure (base <> "/lambdash/allowed")

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
  -- Read it all before returning: allow writes the same file next.
  result <- try (readFile file >>= \text -> evaluate (length text) >> pure text) :: IO (Either IOException String)
  pure $ case result of
    Left _ -> Map.empty
    Right text -> parseAllowed text

writeAllowed :: Map FilePath String -> IO ()
writeAllowed entries = do
  file <- allowedFile
  createDirectoryIfMissing True (reverse (drop 1 (dropWhile (/= '/') (reverse file))))
  writeFile file (renderAllowed entries)

isAllowed :: FilePath -> String -> IO Bool
isAllowed dir hash = (== Just hash) . Map.lookup dir <$> readAllowed

allow :: FilePath -> String -> IO ()
allow dir hash = readAllowed >>= writeAllowed . Map.insert dir hash

deny :: FilePath -> IO ()
deny dir = readAllowed >>= writeAllowed . Map.delete dir
