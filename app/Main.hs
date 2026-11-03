module Main (main) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Lambdash.Nix (withRootDirectory)
import Lambdash.Process (setupTerminal)
import Lambdash.Repl (Session (..), Step (..), initial, needsMore, sessionPrompt, step)
import System.Console.Haskeline
import System.IO (BufferMode (..), hSetBuffering, stdout)

main :: IO ()
main = do
  -- Computations print straight to stdout, beside the output of haskeline.
  hSetBuffering stdout LineBuffering
  setupTerminal
  withRootDirectory $ \roots ->
    runInputT defaultSettings (loop initial {sessionRoots = roots})

-- | Read continuation lines while a @do@ block is open. End of input stops
-- reading, and the parser reports the unclosed block.
more :: String -> InputT IO String
more input
  | needsMore input = do
      next <- getInputLine ".. "
      maybe (pure input) (\line -> more (input <> "\n" <> line)) next
  | otherwise = pure input

-- | All of the meaning is in 'step'. This only moves text.
loop :: Session -> InputT IO ()
loop session = do
  line <- getInputLine (sessionPrompt session) >>= traverse more
  case line of
    Nothing -> pure ()
    Just input -> do
      result <- liftIO (step session input)
      case result of
        Quit -> pure ()
        Continue next output -> do
          unless (null output) (outputStrLn output)
          loop next
