module Main (main) where

import Control.Monad (unless)
import Data.List (isPrefixOf)
import Lambdash.Check (Problem (..), checkText)
import Lambdash.Repl (initial)
import System.Directory (doesFileExist)
import System.Exit (exitFailure)

-- | The problems in a file, as (line, column, message).
problems :: [String] -> [(Int, Maybe Int, String)]
problems source = [(problemLine p, problemColumn p, problemMessage p) | p <- checkText initial (unlines source)]

tests :: [(String, Bool)]
tests =
  [ ("a clean file has no problems", problems ["f = x -> x + 1", "f 2"] == [])
  , ("a type error marks its line", problems ["f = x -> x + 1", "y = f true"] == [(2, Nothing, "no instance: Num Bool")])
  , ("a parse error has its column, without the excerpt",
      case problems ["x = 1", "z = ("] of
        [(2, Just 6, message)] -> "unexpected end of input" `isPrefixOf` message
        _ -> False)
  , ("bindings carry on to later lines", problems ["f = x -> x", "g = y -> f y", "g 1"] == [])
  -- y is then unbound, so the second line is the external command y.
  , ("a failed line binds nothing", problems ["y = 1 + true", "y"] == [(1, Nothing, "no instance: Num Bool")])
  , ("a signature waits for its binding", problems ["f : Int -> Int", "f = x -> x", "f true"] /= [])
  , ("declarations and let rec", problems ["data Maybe a = Nothing | Just a", "let rec len = m -> case m of Nothing -> 0; Just _ -> 1 end", "len (Just 1)"] == [])
  , ("a block spans lines", problems ["f = m -> do", "  print m", "end", "f 1"] == [])
  , ("an error in a block marks every line of it", [(problemLine p, problemEndLine p) | p <- checkText initial (unlines ["f = do", "  print (1 + true)", "end"])] == [(1, 3)])
  ]

main :: IO ()
main = do
  results <- mapM run tests
  -- Checking never runs a command: the file would exist if it did.
  let marker = "check-tests-marker"
  _ <- pure $! length (problems ["touch " <> marker])
  ran <- doesFileExist marker
  safe <- run ("checking runs nothing", not ran)
  let all' = safe : results
  putStrLn (show (length (filter id all')) <> "/" <> show (length all') <> " passed")
  unless (and all') exitFailure
 where
  run (name, passed) = do
    putStrLn ((if passed then "pass  " else "FAIL  ") <> name)
    pure passed
