module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (unless)
import Data.List (isInfixOf, isPrefixOf)
import Lambdash.Check (Problem (..), checkText, hoverAt)
import Lambdash.Repl (Session (..), initial)
import System.Directory (createDirectory, doesFileExist, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.Exit (exitFailure)
import System.IO (hClose, openTempFile)
import System.IO.Unsafe (unsafePerformIO)

-- | The problems in a file, as (line, column, message).
problems :: [String] -> [(Int, Maybe Int, String)]
problems = problemsIn initial

problemsIn :: Session -> [String] -> [(Int, Maybe Int, String)]
problemsIn start source = [(problemLine p, problemColumn p, problemMessage p) | p <- check start (unlines source)]

check :: Session -> String -> [Problem]
check start = unsafePerformIO . checkText start

hover :: [String] -> Int -> Int -> Maybe (Int, Int, Int, String)
hover = hoverIn initial

hoverIn :: Session -> [String] -> Int -> Int -> Maybe (Int, Int, Int, String)
hoverIn start source line column = unsafePerformIO (hoverAt start (unlines source) line column)

-- | Write module files to a new directory, and use a session whose file is
-- in that directory, as the server makes for a file on disk.
withModules :: Show a => [(FilePath, String)] -> (Session -> a) -> a
withModules files use = unsafePerformIO $ do
  base <- getTemporaryDirectory
  (marker, h) <- openTempFile base "check-modules"
  hClose h
  removeFile marker
  let dir = marker <> ".d"
  createDirectory dir
  mapM_ (\(name, text) -> writeFile (dir <> "/" <> name) text) files
  let result = use initial {sessionDirectory = Just dir}
  _ <- evaluate (length (show result))
  removeDirectoryRecursive dir
  pure result

-- | A module with a binding and an operator that is right-associative.
listModule :: String
listModule = "double = x -> x + x\ninfixr 5 <:>\n(<:>) = x xs -> [x] <> xs\n"

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
  , ("a data type declared again is a new type", map (\(line, _, _) -> line) (problems ["data T = A Int", "x = A 1", "data T = A String", "case x of A s -> stringLength s end"]) == [4])
  , ("declarations and let rec", problems ["data Maybe a = Nothing | Just a", "let rec len = m -> case m of Nothing -> 0; Just _ -> 1 end", "len (Just 1)"] == [])
  , ("a block spans lines", problems ["f = m -> do", "  print m", "end", "f 1"] == [])
  , ("hover: a name has its type there", hover ["f = x -> x + 1", "w = f 1"] 2 5 == Just (2, 5, 6, "f : Num a => a -> a"))
  , ("hover: a binding's own name", hover ["g = x -> x"] 1 1 == Just (1, 1, 2, "g : a -> a"))
  , ("hover: a later binding changes it", hover ["f = 1", "f = true", "f"] 3 1 == Just (3, 1, 2, "f : Bool"))
  , ("hover: a use has the type from before its line", hover ["x = true", "x = if x then 1 else 2"] 2 8 == Just (2, 8, 9, "x : Bool") && hover ["x = true", "x = if x then 1 else 2"] 2 1 == Just (2, 1, 2, "x : Int"))
  , ("hover: a let rec member has its new type", fmap (\(_, _, _, t) -> t) (hover ["let rec f = n -> f n"] 1 18) == Just "f : a -> b")
  , ("a signature names a class that exists", check initial "bad : Unknown a => a -> a\n" == [Problem 1 Nothing 1 "no class Unknown"])
  , ("a parse error after a tab is at its character", map problemColumn (check initial "\tx = )\n") == [Just 6])
  , ("hover: a parameter shows nothing", hover ["f = 1", "g = f -> f"] 2 10 == Nothing)
  , ("hover: a builtin and an operator", fmap (\(_, _, _, t) -> t) (hover ["print 1"] 1 3) == Just "print : a -> Comp {console} ()" && fmap (\(_, _, _, t) -> t) (hover ["1 + 2"] 1 3) == Just "(+) : Num a => a -> a -> a")
  , ("hover: nothing on a space or a command", hover ["f 1"] 1 2 == Nothing && hover ["git status"] 1 2 == Nothing)
  , ("an error in a block marks every line of it", [(problemLine p, problemEndLine p) | p <- check initial (unlines ["f = do", "  print (1 + true)", "end"])] == [(1, 3)])
  , ("import: a module's names are known", withModules [("m.lsh", listModule)] (\s -> problemsIn s ["import \"m.lsh\"", "y = double 2"]) == [])
  , ("import: an imported name has its type", withModules [("m.lsh", listModule)] (\s -> problemsIn s ["import \"m.lsh\"", "y = double true"]) == [(2, Nothing, "no instance: Num Bool")]
      && withModules [("m.lsh", listModule)] (\s -> fmap (\(_, _, _, t) -> t) (hoverIn s ["m = import \"m.lsh\"", "m.double 2"] 2 3)) == Just "m.double : Num a => a -> a")
  , ("hover: a field selection is not a prefixed name", fmap (\(_, from, to, _) -> (from, to)) (hover ["r = {x = 1}", "r.x"] 2 1) == Just (1, 2))
  -- Read from the left, 1 <:> 2 would need 2 to be a list.
  , ("import: a module's fixity", withModules [("m.lsh", listModule)] (\s -> problemsIn s ["import \"m.lsh\"", "xs = 1 <:> 2 <:> []"]) == [])
  , ("import: a module that is not there is a problem on its line, and binds nothing",
      withModules [] (\s -> problemsIn s ["x = 1", "import \"missing.lsh\"", "y = x + 1"]) == [(2, Nothing, "no module at missing.lsh")])
  , ("import: a module that fails to load names its line",
      case withModules [("m.lsh", "x = 1\nx = 1 + true\n")] (\s -> problemsIn s ["import \"m.lsh\""]) of
        [(1, Nothing, message)] -> "m.lsh:2: no instance: Num Bool" `isInfixOf` message
        _ -> False)
  , ("import: a module's line that would run stops the load",
      case withModules [("m.lsh", "echo hi\n")] (\s -> problemsIn s ["import \"m.lsh\""]) of
        [(1, Nothing, message)] -> "this line would run" `isInfixOf` message
        _ -> False)
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
