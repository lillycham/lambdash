-- | Environments as values. An overlay is a named, ordered list of patches,
-- and the effective environment is the base with every active overlay applied
-- in the order they were used. Nothing here touches the process: a spawn reads
-- 'effective' and passes it to @execve@.
--
-- > base ⊕ dev ⊕ imagemagick ⊕ ffmpeg
--
-- The fold is ordered and not commutative. Later overlays win, so removing one
-- is deleting it from the list and folding again: no patch needs an inverse.
module Lambdash.Overlay
  ( Environ
  , Op (..)
  , Patch (..)
  , Overlay (..)
  , ShellState (..)
  , emptyState
  , applyPatch
  , applyOverlay
  , effective
  , Shadow (..)
  , shadows
  , useOverlay
  , unuseOverlay
  , Touch (..)
  , touches
  , renderOp
  , renderPatch
  , renderOverlay
  , prompt
  , Runtime (..)
  , newRuntime
  , runtimeEnviron
  ) where

import Data.IORef
import Data.List (intercalate, nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import System.Environment (getEnvironment)

-- | Variable names to values.
type Environ = Map String String

-- | What one patch does to one variable.
data Op
  = Set String
  | Unset
  | -- | Put a value in front, with a separator. An unset variable becomes the
    -- value alone.
    Prepend String String
  | Append String String
  deriving (Eq, Show)

data Patch = Patch String Op
  deriving (Eq, Show)

-- | A named list of patches. They apply in order, so one overlay can set a
-- variable and then prepend to it.
data Overlay = Overlay
  { overlayName :: String
  , overlayPatches :: [Patch]
  }
  deriving (Eq, Show)

-- | The session's environments, oldest first. The base is not stored: it is
-- the process environment, read when something spawns.
newtype ShellState = ShellState {overlays :: [Overlay]}
  deriving (Eq, Show)

emptyState :: ShellState
emptyState = ShellState []

applyPatch :: Patch -> Environ -> Environ
applyPatch (Patch name op) env = case op of
  Set value -> Map.insert name value env
  Unset -> Map.delete name env
  Prepend sep value -> Map.alter (Just . maybe value (\old -> value <> sep <> old)) name env
  Append sep value -> Map.alter (Just . maybe value (\old -> old <> sep <> value)) name env

applyOverlay :: Overlay -> Environ -> Environ
applyOverlay overlay env = foldl (flip applyPatch) env (overlayPatches overlay)

-- | The environment a spawn sees. Later overlays win.
--
-- Note: the plan sketches this as @foldr apply base overlays@, which would make
-- the first overlay win. Here the list is oldest first and the fold is left.
effective :: Environ -> ShellState -> Environ
effective base state = foldl (flip applyOverlay) base (overlays state)

-- | A later overlay replaced a value that an earlier one chose.
data Shadow = Shadow
  { shadowVar :: String
  , shadowEarlier :: String
  , shadowLater :: String
  }
  deriving (Eq, Show)

-- | Every active shadowing, in the order the fold meets it.
--
-- Only a change counts. Every Nix devshell sets @CC@, @LD@ and seventy other
-- stdenv variables, and two devshells on one stdenv set them to the same
-- values: flagging names would flag every second devshell. Prepend and append
-- extend a value instead of replacing it, so they never shadow.
--
-- The base is not an overlay, so overriding it is not shadowing.
shadows :: ShellState -> [Shadow]
shadows state = reverse (snd (foldl layer (Map.empty, []) (overlays state)))
 where
  -- For each variable: the overlay that last chose it, and what it chose.
  layer acc overlay = foldl (patch (overlayName overlay)) acc (overlayPatches overlay)
  patch owner (chosen, found) (Patch name op) = case op of
    Set value -> replace owner (Just value) chosen found name
    Unset -> replace owner Nothing chosen found name
    Prepend _ _ -> (Map.delete name chosen, found)
    Append _ _ -> (Map.delete name chosen, found)
  replace owner value chosen found name =
    let found' = case Map.lookup name chosen of
          Just (earlier, old)
            | earlier /= owner && old /= value -> Shadow name earlier owner : found
          _ -> found
    in (Map.insert name (owner, value) chosen, found')

-- | Activate an overlay. Using a name that is already active replaces it, and
-- the new one goes on top, because the newest overlay should win.
useOverlay :: Overlay -> ShellState -> ShellState
useOverlay overlay (ShellState active) =
  ShellState (filter ((/= overlayName overlay) . overlayName) active <> [overlay])

-- | Deactivate an overlay by name.
unuseOverlay :: String -> ShellState -> Either String ShellState
unuseOverlay name (ShellState active)
  | any ((== name) . overlayName) active = Right (ShellState (filter ((/= name) . overlayName) active))
  | otherwise = Left ("no active environment: " <> name)

-- | One layer's part in a variable's value.
data Touch = Touch
  { touchLayer :: String
  , touchOp :: Op
  }
  deriving (Eq, Show)

-- | Which layers touched a variable, oldest first.
touches :: String -> ShellState -> [Touch]
touches name state =
  [ Touch (overlayName overlay) op
  | overlay <- overlays state
  , Patch var op <- overlayPatches overlay
  , var == name
  ]

-- | The source for a patch, in the syntax that builds it.
renderPatch :: Patch -> String
renderPatch (Patch name op) = renderOp name op

renderOp :: String -> Op -> String
renderOp name op = case op of
  Set value -> "set " <> show name <> " " <> show value
  Unset -> "unset " <> show name
  Prepend ":" value -> "prepend " <> show name <> " " <> show value
  Append ":" value -> "append " <> show name <> " " <> show value
  Prepend sep value -> "prependWith " <> show sep <> " " <> show name <> " " <> show value
  Append sep value -> "appendWith " <> show sep <> " " <> show name <> " " <> show value

renderOverlay :: Overlay -> String
renderOverlay (Overlay name patches) =
  "overlay " <> show name <> " [" <> intercalate ", " (map renderPatch patches) <> "]"

-- | The prompt shows active overlays, and the variables they shadow, because a
-- warning scrolls away and shadowing is ongoing state.
--
-- > λ>
-- > λ [dev bench]>
-- > λ [dev bench ⚠ CC]>
prompt :: ShellState -> String
prompt state = case overlays state of
  [] -> "λ> "
  active ->
    let names = unwords (map overlayName active)
        flagged = nub (map shadowVar (shadows state))
        flag = if null flagged then "" else " ⚠ " <> unwords flagged
    in "λ [" <> names <> flag <> "]> "

-- | What a running computation can reach: the session's environments. A
-- computation that changes them changes this reference, and the REPL commits
-- it only when the line succeeds.
newtype Runtime = Runtime {runtimeState :: IORef ShellState}

newRuntime :: ShellState -> IO Runtime
newRuntime state = Runtime <$> newIORef state

-- | The effective environment now.
runtimeEnviron :: Runtime -> IO Environ
runtimeEnviron runtime = do
  base <- Map.fromList <$> getEnvironment
  effective base <$> readIORef (runtimeState runtime)
