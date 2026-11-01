module Main (main) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Lambdash.Process (setupTerminal)
import Lambdash.Repl (Session, Step (..), initial, step)
import System.Console.Haskeline
import System.IO (BufferMode (..), hSetBuffering, stdout)

main :: IO ()
main = do
  -- Computations print straight to stdout, beside the output of haskeline.
  hSetBuffering stdout LineBuffering
  setupTerminal
  runInputT defaultSettings (loop initial)

-- | All of the meaning is in 'step'. This only moves text.
loop :: Session -> InputT IO ()
loop session = do
  line <- getInputLine "λ> "
  case line of
    Nothing -> pure ()
    Just input -> do
      result <- liftIO (step session input)
      case result of
        Quit -> pure ()
        Continue next output -> do
          unless (null output) (outputStrLn output)
          loop next
