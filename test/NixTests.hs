-- | The pure parts of the Nix resolvers: turning Nix's output into overlays.
-- test/nix.exp runs real Nix against test/nix-fixture.
module Main (main) where

import Control.Monad (unless)
import Data.ByteString.Char8 qualified as B8
import Data.Map.Strict qualified as Map
import Lambdash.Nix
import Lambdash.Overlay
import Lambdash.Trust (autoName, parseAllowed, renderAllowed)
import System.Exit (exitFailure)

env :: [(String, String)] -> Environ
env = Map.fromList

diff :: [(String, String)] -> [(String, String)] -> [Patch]
diff before after = diffEnviron (env before) (env after)

outer, inner :: Environ
outer = env [("PATH", "/bin"), ("HOME", "/h"), ("GONE", "x")]
inner = env [("PATH", "/nix/bin:/bin"), ("HOME", "/h"), ("CC", "gcc")]

script :: String
script = unlines ["export X=1", "eval \"${shellHook:-}\""]

tests :: [(String, Bool)]
tests =
  [ ("diff: nothing changed", null (diff [("A", "1")] [("A", "1")]))
  , ("diff: a new variable is a set", diff [] [("A", "1")] == [Patch "A" (Set "1")])
  , ("diff: a changed value is a set", diff [("A", "1")] [("A", "2")] == [Patch "A" (Set "2")])
  , ("diff: a removed variable is an unset", diff [("A", "1")] [] == [Patch "A" Unset])
  , ("diff: a joined PATH is a prepend", diff [("PATH", "/usr/bin:/bin")] [("PATH", "/nix/a/bin:/nix/b/bin:/usr/bin:/bin")] == [Patch "PATH" (Prepend ":" "/nix/a/bin:/nix/b/bin")])
  , ("diff: an extended value is an append", diff [("X", "/a")] [("X", "/a:/b")] == [Patch "X" (Append ":" "/b")])
  , ("diff: a prefix without a separator is a set", diff [("X", "bin")] [("X", "/nixbin")] == [Patch "X" (Set "/nixbin")])
  , ("diff: an empty old value is a set", diff [("X", "")] [("X", ":a")] == [Patch "X" (Set ":a")])
  , ("diff: a prepend replays onto a changed base", applyOverlay (Overlay "d" (diff [("PATH", "/bin")] [("PATH", "/nix/bin:/bin")])) (env [("PATH", "/opt/bin:/bin")]) Map.! "PATH" == "/nix/bin:/opt/bin:/bin")
  , ("diff: the temporary directory is dropped", null (diff [("TMPDIR", "/tmp")] [("TMPDIR", "/tmp/nix-shell.abc")]))
  , ("diff: dropped names are never unset", null (diff [("SHELL", "/bin/zsh"), ("PWD", "/x")] []))
  , ("diff: noise is dropped", null (diff [] [("out", "/x/outputs/out"), ("name", "nix-shell-env"), ("shellHook", "echo hi"), ("SHLVL", "2"), ("_", "/bin/env")]))
  , ("diff: patches are in name order", map (\(Patch name _) -> name) (diff [] [("B", "1"), ("A", "1"), ("C", "1")]) == ["A", "B", "C"])
  , ("diff: replaying gives the devshell environment", applyOverlay (Overlay "d" (diffEnviron outer inner)) outer == inner)
  , ("env0: pairs", parseEnv0 (B8.pack "A=1\0B=two words\0") == env [("A", "1"), ("B", "two words")])
  , ("env0: a value with =", parseEnv0 (B8.pack "A=x=y\0") == env [("A", "x=y")])
  , ("env0: an empty value", parseEnv0 (B8.pack "A=\0") == env [("A", "")])
  , ("env0: a newline in a value", parseEnv0 (B8.pack "A=1\n2\0") == env [("A", "1\n2")])
  , ("env0: UTF-8", parseEnv0 (B8.pack "A=\206\187\0") == env [("A", "λ")])
  , ("env0: an entry without = is skipped", parseEnv0 (B8.pack "junk\0A=1\0") == env [("A", "1")])
  , ("pkg: one output", pkgOverlay "nixpkgs#hello" ["/nix/store/h"] == Overlay "nixpkgs#hello" [Patch "PATH" (Prepend ":" "/nix/store/h/bin")])
  , ("pkg: the first output ends up first", applyOverlay (pkgOverlay "jq" ["/s/jq-bin", "/s/jq-man"]) (env [("PATH", "/bin")]) Map.! "PATH" == "/s/jq-bin/bin:/s/jq-man/bin:/bin")
  , ("pkg: blank lines are ignored", pkgOverlay "x" ["/s/a", ""] == pkgOverlay "x" ["/s/a"])
  , ("pkg: only PATH changes", all (\(Patch name _) -> name == "PATH") (overlayPatches (pkgOverlay "x" ["/a", "/b"])))
  , ("hook: the marker line records functions first", lines (markHook "/t/before" script) !! 1 == "declare -F > \"/t/before\"; " <> hookMarker)
  , ("hook: other lines are untouched", take 1 (lines (markHook "/t/before" script)) == ["export X=1"])
  , ("hook: no marker, no change", markHook "/t/before" "echo hi\n" == "echo hi\n")
  , ("hook: new functions", newFunctions "declare -f a\ndeclare -f b\n" "declare -f a\ndeclare -f b\ndeclare -f mine\ndeclare -f also\n" == ["also", "mine"])
  , ("trust: the allowed file round-trips", parseAllowed (renderAllowed (Map.fromList [("/a b", "h1"), ("/c", "h2")])) == Map.fromList [("/a b", "h1"), ("/c", "h2")])
  , ("trust: a malformed line is skipped", parseAllowed "junk\nh1 /a\n" == Map.fromList [("/a", "h1")])
  , ("trust: an automatic overlay is named for its directory", autoName "/home/l/akyuu" == "akyuu#" && autoName "/" == "/#")
  , ("hook: no new functions", null (newFunctions "declare -f a\n" "declare -f a\n"))
  ]

main :: IO ()
main = do
  results <- mapM run tests
  putStrLn (show (length (filter id results)) <> "/" <> show (length results) <> " passed")
  unless (and results) exitFailure
 where
  run (name, passed) = do
    putStrLn ((if passed then "pass  " else "FAIL  ") <> name)
    pure passed
