-- | The overlay model: the fold, precedence, removal, shadowing and the prompt.
-- Everything here is pure.
module Main (main) where

import Control.Monad (unless)
import Data.Map.Strict qualified as Map
import Lambdash.Overlay
import System.Exit (exitFailure)

base :: Environ
base = Map.fromList [("PATH", "/usr/bin"), ("HOME", "/home/l"), ("CC", "cc")]

dev, bench, ffmpeg, same :: Overlay
dev = Overlay "dev" [Patch "CC" (Set "gcc"), Patch "PATH" (Prepend ":" "/dev/bin")]
bench = Overlay "bench" [Patch "CC" (Set "clang")]
ffmpeg = Overlay "ffmpeg" [Patch "PATH" (Prepend ":" "/ffmpeg/bin")]
same = Overlay "same" [Patch "CC" (Set "gcc")]

active :: [Overlay] -> ShellState
active = foldl (flip useOverlay) emptyState

env :: [Overlay] -> Environ
env = effective base . active

var :: String -> [Overlay] -> Maybe String
var name = Map.lookup name . env

tests :: [(String, Bool)]
tests =
  [ ("fold: no overlays is the base", env [] == base)
  , ("patch: set", applyPatch (Patch "X" (Set "1")) Map.empty == Map.fromList [("X", "1")])
  , ("patch: set replaces", applyPatch (Patch "CC" (Set "gcc")) base Map.! "CC" == "gcc")
  , ("patch: unset", Map.notMember "CC" (applyPatch (Patch "CC" Unset) base))
  , ("patch: unset of an unset variable", applyPatch (Patch "NOPE" Unset) base == base)
  , ("patch: prepend", applyPatch (Patch "PATH" (Prepend ":" "/a")) base Map.! "PATH" == "/a:/usr/bin")
  , ("patch: append", applyPatch (Patch "PATH" (Append ":" "/a")) base Map.! "PATH" == "/usr/bin:/a")
  , ("patch: prepend to an unset variable is the value alone", applyPatch (Patch "X" (Prepend ":" "/a")) Map.empty == Map.fromList [("X", "/a")])
  , ("patch: another separator", applyPatch (Patch "X" (Append " " "-O2")) (Map.fromList [("X", "-g")]) Map.! "X" == "-g -O2")
  , ("overlay: patches apply in order", applyOverlay (Overlay "o" [Patch "X" (Set "a"), Patch "X" (Prepend ":" "b")]) Map.empty == Map.fromList [("X", "b:a")])
  , ("overlay: a later patch in one overlay wins", applyOverlay (Overlay "o" [Patch "X" (Set "a"), Patch "X" (Set "b")]) Map.empty == Map.fromList [("X", "b")])
  , ("fold: the later overlay wins", var "CC" [dev, bench] == Just "clang")
  , ("fold: order matters", var "CC" [bench, dev] == Just "gcc")
  , ("fold: prepends stack, newest first", var "PATH" [dev, ffmpeg] == Just "/ffmpeg/bin:/dev/bin:/usr/bin")
  , ("fold: untouched variables pass through", var "HOME" [dev, bench, ffmpeg] == Just "/home/l")
  , ("use: appends", map overlayName (overlays (active [dev, bench])) == ["dev", "bench"])
  , ("use: the same name replaces, and goes on top", map overlayName (overlays (active [dev, bench, dev])) == ["bench", "dev"])
  , ("use: replacing changes the patches", var "CC" [dev, Overlay "dev" [Patch "CC" (Set "tcc")]] == Just "tcc")
  , ("unuse: removes, and the fold forgets it", (effective base <$> unuseOverlay "bench" (active [dev, bench])) == Right (env [dev]))
  , ("unuse: the middle one", (effective base <$> unuseOverlay "bench" (active [dev, bench, ffmpeg])) == Right (env [dev, ffmpeg]))
  , ("unuse: everything gives the base", (effective base <$> (unuseOverlay "dev" =<< unuseOverlay "ffmpeg" (active [dev, ffmpeg]))) == Right base)
  , ("unuse: an unknown name fails", unuseOverlay "nope" (active [dev]) == Left "no active environment: nope")
  , ("shadows: none for one overlay", null (shadows (active [dev])))
  , ("shadows: a different value", shadows (active [dev, bench]) == [Shadow "CC" "dev" "bench"])
  , ("shadows: the same value is not shadowing", null (shadows (active [dev, same])))
  , ("shadows: prepends never shadow", null (shadows (active [dev, ffmpeg])))
  , ("shadows: unset shadows a set", shadows (active [dev, Overlay "u" [Patch "CC" Unset]]) == [Shadow "CC" "dev" "u"])
  , ("shadows: one overlay never shadows itself", null (shadows (active [Overlay "o" [Patch "X" (Set "a"), Patch "X" (Set "b")]])))
  , ("shadows: overriding the base is not shadowing", null (shadows (active [bench])))
  , ("shadows: a prepend in between resets the owner", null (shadows (active [dev, Overlay "p" [Patch "CC" (Prepend " " "ccache")], bench])))
  , ("shadows: a chain", shadows (active [dev, bench, Overlay "t" [Patch "CC" (Set "tcc")]]) == [Shadow "CC" "dev" "bench", Shadow "CC" "bench" "t"])
  , ("shadows: unuse clears it", either (const False) (null . shadows) (unuseOverlay "bench" (active [dev, bench])))
  , ("touches: oldest first", touches "CC" (active [dev, ffmpeg, bench]) == [Touch "dev" (Set "gcc"), Touch "bench" (Set "clang")])
  , ("touches: none", null (touches "HOME" (active [dev, bench])))
  , ("prompt: plain", prompt emptyState == "λ> ")
  , ("prompt: active environments", prompt (active [dev, ffmpeg]) == "λ [dev ffmpeg]> ")
  , ("prompt: shadowing", prompt (active [dev, bench]) == "λ [dev bench ⚠ CC]> ")
  , ("prompt: a variable is flagged once", prompt (active [dev, bench, Overlay "t" [Patch "CC" (Set "tcc")]]) == "λ [dev bench t ⚠ CC]> ")
  , ("render: an overlay is its own source", renderOverlay dev == "overlay \"dev\" [set \"CC\" \"gcc\", prepend \"PATH\" \"/dev/bin\"]")
  , ("render: another separator", renderPatch (Patch "X" (Append " " "-O2")) == "appendWith \" \" \"X\" \"-O2\"")
  , ("render: unset", renderPatch (Patch "X" Unset) == "unset \"X\"")
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
