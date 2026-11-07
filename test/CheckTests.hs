module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (unless)
import Data.List (isInfixOf, isPrefixOf)
import Lambdash.Check (Problem (..), checkText, hoverAt)
import Lambdash.Navigate
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

-- | The outline as (name, sort, line, first column, column after), each
-- symbol followed by what it holds.
symbols :: [String] -> [(String, SymbolSort, Int, Int, Int)]
symbols source = concatMap flat (unsafePerformIO (outline initial (unlines source)))
 where
  flat s = (symbolName s, symbolSort s, symbolLine s, symbolFrom s, symbolTo s) : concatMap flat (symbolChildren s)

definition :: [String] -> Int -> Int -> Maybe (Int, Int, Int)
definition source line column = fmap (\p -> (placeLine p, placeFrom p, placeTo p)) (definitionIn initial source line column)

definitionIn :: Session -> [String] -> Int -> Int -> Maybe Place
definitionIn start source line column = unsafePerformIO (definitionAt start (unlines source) line column)

completions :: Session -> [String] -> Int -> Int -> (Int, [Suggestion])
completions start source line column = unsafePerformIO (completionsAt start (unlines source) line column)

typed :: [String] -> Int -> Int -> Maybe String
typed source line column = (\(_, _, _, t) -> t) <$> unsafePerformIO (typeAt initial (unlines source) line column)

offered :: Session -> [String] -> Int -> Int -> [String]
offered start source line column = map suggestionText (snd (completions start source line column))

lensesOf :: [String] -> [(Int, Int, String)]
lensesOf source = [(lensFirst l, lensLast l, lensEffects l) | l <- unsafePerformIO (lenses initial (unlines source))]

-- | What fits the first hole of a file.
fills :: [String] -> [String]
fills source = concat (take 1 [problemFills p | p <- check initial (unlines source), not (null (problemFills p))])

hints :: [String] -> [Hint]
hints source = unsafePerformIO (inlayHints initial (unlines source))

-- | Two functions that call each other: each one's first place is in the
-- other's body or its own name.
evenOdd :: String
evenOdd = "let rec even = n -> if n == 0 then true else odd (n - 1) and odd = n -> if n == 0 then false else even (n - 1)"

-- | A module with a binding and an operator that is right-associative.
listModule :: String
listModule = "double = x -> x + x\ninfixr 5 <:>\n(<:>) = x xs -> [x] <> xs\n"

tests :: [(String, Bool)]
tests =
  [ ("a clean file has no problems", problems ["f = x -> x + 1", "f 2"] == [])
  , ("a missing instance is where the dictionary was asked for", problems ["f = x -> x + 1", "y = f true"] == [(2, Just 5, "no instance: Num Bool")])
  , ("a hole is a problem at its place, with its type", [(l, c, takeWhile (/= '\n') m) | (l, c, m) <- problems ["x = stringLength _"]] == [(1, Just 18, "hole: String")])
  , ("fits: a name of the right type, and refinements after the names",
      let found = fills ["title = \"hi\"", "n = stringLength _"]
      in take 1 found == ["title"] && "(show _)" `elem` found)
  , ("fits: nothing that needs an instance that is missing", "(div _ _)" `notElem` fills ["n = stringLength _"])
  , ("fits: a name the line binds comes first", take 1 (fills ["title = \"hi\"", "f = n -> stringLength _"]) == ["n"])
  , ("fits: a record's fields, for a hole that takes it",
      all (`elem` fills ["r <- each [{a = 1, b = \"x\"}] | map _ | collect"]) [".a", ".b"])
  , ("fits: a binding with a signature, and a refinement that leaves a hole",
      "(stringLength _)" `elem` fills ["y : Int", "y = _"])
  , ("each hole is a problem of its own",
      [(problemColumn p, takeWhile (/= '\n') (problemMessage p)) | p <- check initial "y = stringLength _ + _\n"] == [(Just 18, "hole: String"), (Just 22, "hole: Int")])
  , ("a mismatch is at the argument that does not fit",
      [(problemLine p, problemColumn p, problemEndColumn p) | p <- check initial "x = stringLength true\n"] == [(1, Just 18, Just 22)])
  , ("an annotation that does not fit is at its expression",
      [(problemColumn p, problemEndColumn p) | p <- check initial "x = (true : Int)\n"] == [(Just 6, Just 10)])
  , ("applying what is not a function is at the application",
      [(problemColumn p, problemEndColumn p) | p <- check initial "x = true 1\n"] == [(Just 5, Just 11)])
  , ("a parse error has its column, without the excerpt",
      case problems ["x = 1", "z = ("] of
        [(2, Just 6, message)] -> "unexpected end of input" `isPrefixOf` message
        _ -> False)
  , ("bindings carry on to later lines", problems ["f = x -> x", "g = y -> f y", "g 1"] == [])
  -- y is then unbound, so the second line is the external command y.
  , ("a failed line binds nothing", problems ["y = 1 + true", "y"] == [(1, Just 5, "no instance: Num Bool")])
  , ("a signature waits for its binding", problems ["f : Int -> Int", "f = x -> x", "f true"] /= [])
  , ("a data type declared again is a new type", map (\(line, _, _) -> line) (problems ["data T = A Int", "x = A 1", "data T = A String", "case x of A s -> stringLength s end"]) == [4])
  , ("declarations and let rec", problems ["data Maybe a = Nothing | Just a", "let rec len = m -> case m of Nothing -> 0; Just _ -> 1 end", "len (Just 1)"] == [])
  , ("a block spans lines", problems ["f = m -> do", "  print m", "end", "f 1"] == [])
  , ("hover: a name has its type there", hover ["f = x -> x + 1", "w = f 1"] 2 5 == Just (2, 5, 6, "f : Num a => a -> a"))
  , ("hover: a binding's own name", hover ["g = x -> x"] 1 1 == Just (1, 1, 2, "g : a -> a"))
  , ("hover: a later binding changes it", hover ["f = 1", "f = true", "f"] 3 1 == Just (3, 1, 2, "f : Bool"))
  , ("hover: a use has the type from before its line", hover ["x = true", "x = if x then 1 else 2"] 2 8 == Just (2, 8, 9, "x : Bool") && hover ["x = true", "x = if x then 1 else 2"] 2 1 == Just (2, 1, 2, "x : Int"))
  , ("hover: a let rec member has its new type", fmap (\(_, _, _, t) -> t) (hover ["let rec f = n -> f n"] 1 18) == Just "f : a -> b")
  , ("a signature names a class that exists", check initial "bad : Unknown a => a -> a\n" == [Problem 1 Nothing 1 "no class Unknown" Nothing []])
  , ("a parse error after a tab is at its character", map problemColumn (check initial "\tx = )\n") == [Just 6])
  , ("hover: a parameter shows nothing", hover ["f = 1", "g = f -> f"] 2 10 == Nothing)
  , ("hover: a builtin and an operator", fmap (\(_, _, _, t) -> t) (hover ["print 1"] 1 3) == Just "print : a -> Comp {console} ()" && fmap (\(_, _, _, t) -> t) (hover ["1 + 2"] 1 3) == Just "(+) : Num a => a -> a -> a")
  , ("hover: nothing on a space or a command", hover ["f 1"] 1 2 == Nothing && hover ["git status"] 1 2 == Nothing)
  , ("an error in a block is on its own line, not the whole block", [(problemLine p, problemColumn p, problemEndLine p) | p <- check initial (unlines ["f = do", "  print (1 + true)", "end"])] == [(2, Just 10, 2)])
  , ("import: a module's names are known", withModules [("m.lsh", listModule)] (\s -> problemsIn s ["import \"m.lsh\"", "y = double 2"]) == [])
  , ("import: an imported name has its type", withModules [("m.lsh", listModule)] (\s -> problemsIn s ["import \"m.lsh\"", "y = double true"]) == [(2, Just 5, "no instance: Num Bool")]
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
  , ("outline: bindings, a data type and a class, with their places and sorts",
      symbols ["f = x -> x + 1", "n = 2", "data Shape = Circle Int | Square Int", "class Size a where size : a -> Int"]
        == [ ("f", FunctionSymbol, 1, 1, 2), ("n", ValueSymbol, 2, 1, 2)
           , ("Shape", TypeSymbol, 3, 6, 11), ("Circle", ConstructorSymbol, 3, 14, 20), ("Square", ConstructorSymbol, 3, 27, 33)
           , ("Size", ClassSymbol, 4, 7, 11), ("size", MethodSymbol, 4, 20, 24) ])
  , ("outline: a let rec member is where it is named, not where it is used",
      symbols [evenOdd] == [("even", FunctionSymbol, 1, 9, 13), ("odd", FunctionSymbol, 1, 62, 65)])
  , ("outline: a binding has its type, and a failed line defines nothing",
      fmap symbolDetail (unsafePerformIO (outline initial "f = x -> x + 1\ny = 1 + true\n")) == [Just "Num a => a -> a"])
  , ("outline: an instance shows its head", [(name, sort') | (name, sort', _, _, _) <- symbols ["data T = T", "instance Show T where show = _ -> \"T\""]] == [("T", TypeSymbol), ("T", ConstructorSymbol), ("instance Show T", InstanceSymbol)])
  , ("definition: a use goes to the definition it sees", definition ["x = true", "x = if x then 1 else 2"] 2 8 == Just (1, 1, 2))
  , ("definition: a binding's own name is its definition", definition ["x = true", "x = if x then 1 else 2"] 2 1 == Just (2, 1, 2))
  , ("definition: a constructor and a let rec member", definition ["data Shape = Circle Int", "c = Circle 1"] 2 5 == Just (1, 14, 20) && definition [evenOdd] 1 46 == Just (1, 62, 65))
  , ("definition: a builtin goes nowhere", definition ["print 1"] 1 2 == Nothing)
  , ("definition: a parameter goes to its lambda, and a binder is its own definition",
      definition ["f = x -> x + 1"] 1 10 == Just (1, 5, 6) && definition ["f = x -> x"] 1 5 == Just (1, 5, 6))
  , ("definition: each parameter of a lambda", definition ["f = x y -> y + x"] 1 12 == Just (1, 7, 8) && definition ["f = x y -> y + x"] 1 16 == Just (1, 5, 6))
  , ("definition: an inner parameter shadows an outer one", definition ["f = x -> (x -> x)"] 1 16 == Just (1, 11, 12))
  , ("definition: a let and a pattern variable",
      definition ["y = let a = 1 in a + 2"] 1 18 == Just (1, 9, 10)
        && definition ["data Box = Box Int", "f = b -> case b of Box v -> v + 1 end"] 2 29 == Just (2, 24, 25)
        && definition ["data Box = Box Int", "f = b -> case b of Box v -> v + 1 end"] 2 15 == Just (2, 5, 6))
  , ("definition: a <- in a block, across its lines", definition ["f = do", "  d <- pwd;", "  print d", "end"] 3 9 == Just (2, 3, 4))
  , ("definition: a let is not recursive, so its value sees the parameter it shadows",
      definition ["f = x -> let x = x + 1 in x"] 1 18 == Just (1, 5, 6) && definition ["f = x -> let x = x + 1 in x"] 1 27 == Just (1, 14, 15))
  , ("definition: a local let rec names itself",
      definition ["y = let rec go = n -> go n in go 1"] 1 31 == Just (1, 13, 15)
        && definition ["y = let rec go = n -> go n in go 1"] 1 23 == Just (1, 13, 15)
        && definition ["y = let rec go = n -> go n in go 1"] 1 26 == Just (1, 18, 19))
  , ("definition: a parameter before a _ still has its place",
      definition ["f = title width _ -> width"] 1 23 == Just (1, 11, 16) && definition ["f = title width _ -> title"] 1 23 == Just (1, 5, 10))
  , ("definition: a $name in a command", definition ["f = name -> echo $name"] 1 19 == Just (1, 5, 9))
  , ("definition: a top-level name inside a lambda is still top-level", definition ["g = 1", "f = x -> g + x"] 2 10 == Just (1, 1, 2))
  , ("definition: an imported name goes to its module",
      withModules [("m.lsh", listModule)] (\s -> fmap (\p -> (fmap (reverse . take 6 . reverse) (placeFile p), placeLine p, placeFrom p)) (definitionIn s ["m = import \"m.lsh\"", "m.double 2"] 2 3))
        == Just (Just "/m.lsh", 1, 1)
      && withModules [("m.lsh", listModule)] (\s -> fmap placeLine (definitionIn s ["import \"m.lsh\"", "double 2"] 2 2)) == Just 1)
  , ("definition: through a module that imports another",
      withModules [("m.lsh", listModule), ("n.lsh", "import \"m.lsh\"\n")] (\s -> fmap (\p -> (fmap (reverse . take 6 . reverse) (placeFile p), placeLine p)) (definitionIn s ["import \"n.lsh\"", "double 2"] 2 2))
        == Just (Just "/m.lsh", 1))
  , ("completion: a name with its type, from where the word starts",
      case completions initial ["padRight = w t -> t", "x = pa"] 2 7 of
        (5, found) -> Suggestion "padRight" FunctionSuggestion (Just "a -> b -> b") `elem` found
        _ -> False)
  , ("completion: a module's names and a record's fields",
      withModules [("m.lsh", listModule)] (\s -> offered s ["m = import \"m.lsh\"", "m.d"] 2 4) == ["m.double"]
        && offered initial ["r = {name = 1}", "r.na"] 2 5 == ["r.name"])
  , ("completion: the chunk's own words and the reserved words",
      "value" `elem` offered initial ["f = value -> val"] 1 17 && "rec" `elem` offered initial ["let re"] 1 7)
  , ("hints: a binding without a signature shows its type after its name, and the signature that says it",
      hints ["f = x -> x + 1"] == [Hint 1 2 "Num a => a -> a" (Just (1, "f : Num a => a -> a\n"))])
  , ("hints: a binding with a signature shows nothing", hints ["f : Int -> Int", "f = x -> x + 1"] == [])
  , ("hints: a <- binding and an operator have a type but no signature",
      map hintSignature (hints ["y <- pwd", "(<+>) = a b -> a + b"]) == [Nothing, Nothing] && length (hints ["y <- pwd", "(<+>) = a b -> a + b"]) == 2)
  , ("hints: each written signature checks, with effects, records and streams",
      let source = ["g = s -> do print s; print s end", "r = {name = \"a\", size = 2}", "h = xs -> xs | map (x -> x)", evenOdd]
          signed = foldr insert source [sig | Hint _ _ _ (Just sig) <- hints source]
          insert (line, text) ls = take (line - 1) ls <> lines text <> drop (line - 1) ls
      in length [() | Hint _ _ _ (Just _) <- hints source] == 5 && problems signed == [] && hints signed == [])
  , ("lenses: each line that runs has its effects, and a definition has none",
      lensesOf ["f = x -> x + 1", "print 3", "git status", "h <- pwd", "cd /tmp"]
        == [(2, 2, "{console}"), (3, 3, "{exec git}"), (4, 4, "{exec pwd}"), (5, 5, "{cwd}")])
  , ("lenses: a value is pure, and a block is one lens over its lines",
      lensesOf ["1 + 2", "do", "  print 1;", "  echo hi", "end"] == [(1, 1, "pure"), (2, 5, "{exec echo, console}")])
  , ("lenses: a line that fails has none", lensesOf ["y = 1 + true", "print (1 + true)"] == [])
  , ("hover: a parameter has its type, at a use and at its binder",
      typed ["g = n -> stringLength n"] 1 23 == Just "n : String" && typed ["g = n -> stringLength n"] 1 5 == Just "n : String")
  , ("hover: a parameter keeps its class context", typed ["f = x -> x + 1"] 1 10 == Just "x : Num a => a")
  , ("hover: the inner of two parameters of one name", typed ["f = x -> (x -> stringLength x)"] 1 29 == Just "x : String" && typed ["f = x -> (x -> stringLength x)"] 1 11 == Just "x : String" && typed ["f = x -> (x -> stringLength x)"] 1 5 == Nothing)
  , ("hover: a pattern variable", typed ["data Box = Box Int", "f = b -> case b of Box v -> v + 1 end"] 2 29 == Just "v : Int")
  , ("hover: a binder nothing uses shows nothing", typed ["f = x -> 1"] 1 5 == Nothing)
  , ("hover: a top-level name still has its session type", typed ["f = x -> x", "g = f 1"] 2 5 == Just "f : a -> a")
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
