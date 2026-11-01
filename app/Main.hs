module Main (main) where

import Control.Monad (unless)
import Lambdash.Repl (Session, Step (..), initial, step)
import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings (loop initial)

-- | All of the meaning is in 'step'. This only moves text.
loop :: Session -> InputT IO ()
loop session = do
  line <- getInputLine "λ> "
  case fmap (step session) line of
    Nothing -> pure ()
    Just Quit -> pure ()
    Just (Continue next output) -> do
      unless (null output) (outputStrLn output)
      loop next
