-- | REPL tests. Each runs lines through 'step', as the loop does, and compares
-- the text it would show.
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (unless, when)
import Data.List (isInfixOf)
import Data.Set qualified as Set
import Lambdash.Desugar (stripLocations)
import Lambdash.Infer
import Lambdash.Parser (Bound (..))
import Lambdash.Repl
import Lambdash.Unify
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getCurrentDirectory, getTemporaryDirectory, removeDirectoryRecursive, removeFile, setCurrentDirectory)
import System.Exit (exitFailure)
import System.IO (IOMode (..), hClose, hFlush, hPutStr, openFile, openTempFile, stdin, stdout)
import System.Environment (lookupEnv, setEnv)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)


-- | Run lines in one session, and collect what each shows. Stop at 'Quit'.
sessionIO :: [String] -> IO [String]
sessionIO = go initial
 where
  go _ [] = pure []
  go s (l : ls) = do
    result <- step s l
    case result of
      Quit -> pure ["<quit>"]
      Continue next out -> (out :) <$> go next ls
      Failed out -> (out :) <$> go s ls

-- | The prompt after some lines.
promptAfter :: [String] -> String
promptAfter = unsafePerformIO . go initial
 where
  go s [] = sessionPrompt <$> getCurrentDirectory <*> pure s
  go s (l : ls) = do
    result <- step s l
    case result of
      Quit -> pure "<quit>"
      Continue next _ -> go next ls
      Failed _ -> go s ls

-- | Tests are plain values, so this hides the 'IO'. Each call is one session.
session :: [String] -> [String]
session = unsafePerformIO . sessionIO

-- | What a session sends to stdout, beside what its lines show.
capture :: IO a -> IO (String, a)
capture action = do
  hFlush stdout
  saved <- hDuplicate stdout
  (path, handle) <- openTempFile "." "repl-test.out"
  hDuplicateTo handle stdout
  hClose handle
  result <- action
  hFlush stdout
  hDuplicateTo saved stdout
  hClose saved
  text <- readFile path
  _ <- evaluate (length text)
  removeFile path
  pure (text, result)

-- | Both what a session prints, and what its lines show.
prints :: [String] -> (String, [String])
prints = unsafePerformIO . capture . sessionIO
-- | What the last line shows.
final :: [String] -> String
final = last . session

-- | A command without its spans, so that tests compare shapes.
parsed :: Bound -> String -> Either String Command
parsed bound line = strip <$> parseCommand bound line
 where
  strip command = case command of
    TypeOf e -> TypeOf (stripLocations e)
    Bind name e -> Bind name (stripLocations e)
    BindRun name e -> BindRun name (stripLocations e)
    Run e -> Run (stripLocations e)
    other -> other

names :: Bound
names = BoundNames (Set.fromList ["f", "x", "y"])

-- | With the builtins whose signatures guide parsing.
sigs :: Bound
sigs = BoundWith (Set.fromList ["f", "x", "y", "ls", "cd", "collect"]) (Set.fromList ["ls", "cd", "collect"])

ty :: String -> String -> Bool
ty input expected = session [":t " <> input] == [expected]

v0, v1 :: Var
v0 = Var 7
v1 = Var 3

-- | Run a startup file, then lines at the prompt: the file's reports, with
-- its path left out, and what each line shows.
scripted :: String -> [String] -> ([String], [String])
scripted text after = unsafePerformIO $ do
  dir <- getTemporaryDirectory
  (path, h) <- openTempFile dir "init.lsh"
  hPutStr h text
  hClose h
  (ready, reports) <- runScript path initial
  removeFile path
  shown <- go ready after
  pure (map (\r -> if take (length path) r == path then drop (length path) r else r) reports, shown)
 where
  go _ [] = pure []
  go s (l : ls) = do
    result <- step s l
    case result of
      Quit -> pure ["<quit>"]
      Continue next out -> (out :) <$> go next ls
      Failed out -> (out :) <$> go s ls

tests :: [(String, Bool)]
tests =
  [ ("command: blank", parsed names "" == Right Blank && parsed names "  -- note" == Right Blank)
  , ("command: quit", parsed names ":q" == Right Exit)
  , ("command: an expression", parsed names "x" == Right (Run (EVar "x")))
  , ("command: an application is not a binding", parsed names "f x" == Right (Run (EApp (EVar "f") (EVar "x"))))
  , ("command: a binding", parsed names "x = 1" == Right (Bind "x" (ELit (LInt 1))))
  , ("command: a lambda binding", parsed names "i = x -> x" == Right (Bind "i" (ELam "x" (EVar "x"))))
  , ("command: a type query", parsed names ":t x" == Right (TypeOf (EVar "x")))
  , ("command: let is still an expression", parsed names "let x = 1 in x" == Right (Run (ELet "x" (ELit (LInt 1)) (EVar "x"))))
  , ("command: commands in a block", parsed names "do cabal build; cabal test end" == Right (Run (EBind "%1" (ECmd "cabal" [AWord "build"]) (ERun (ECmd "cabal" [AWord "test"])))))
  , ("command: end ends a command line in a block", parsed names "do echo a end" == Right (Run (ERun (ECmd "echo" [AWord "a"]))))
  , ("command: end is a word outside a block", parsed names "echo end" == Right (Run (ECmd "echo" [AWord "end"])))
  , ("command: ; ends a bareword", parsed names "do echo a;echo b end" == Right (Run (EBind "%1" (ECmd "echo" [AWord "a"]) (ERun (ECmd "echo" [AWord "b"])))))
  , ("command: a block binding is not a command", parsed names "do g <- f; g 1 end" == Right (Run (EBind "g" (EVar "f") (ERun (EApp (EVar "g") (ELit (LInt 1)))))))
  , ("command: jobs", parsed names "jobs" == Right ListJobs)
  , ("command: fg", parsed names "fg" == Right (Foreground Nothing))
  , ("command: fg with a number", parsed names "fg 2" == Right (Foreground (Just 2)) && parsed names "fg %2" == Right (Foreground (Just 2)))
  , ("command: a bound fg is a value", parsed (BoundNames (Set.fromList ["fg"])) "fg" == Right (Run (EVar "fg")))
  , ("command: fg with a word is a command line", parsed names "fg x" == Right (Run (ECmd "fg" [AWord "x"])))
  , ("step: fg with no jobs", session ["fg"] == ["no jobs"] && session ["fg 3"] == ["no job 3"])
  , ("step: jobs with no jobs", session ["jobs"] == [""])
  , ("command: bg", parsed names "bg" == Right (Background Nothing) && parsed names "bg %3" == Right (Background (Just 3)))
  , ("step: bg with no jobs", session ["bg"] == ["no stopped jobs"] && session ["bg 2"] == ["no stopped job 2"])
  , ("step: :q with no jobs quits at once", session [":q"] == ["<quit>"])
  , ("command: an unknown command", either (const True) (const False) (parsed names ":nope"))
  , ("command: a binding needs a value", either (const True) (const False) (parsed names "x ="))
  , ("renderType: concrete", renderType TInt == "Int")
  , ("renderType: unknowns from first seen", renderType (TFun (TVar v0) (TVar v1)) == "a -> b")
  , ("renderType: a repeated unknown", renderType (TFun (TVar v0) (TVar v0)) == "a -> a")
  , ("renderType: arrows go to the right", renderType (TFun TInt (TFun TBool TInt)) == "Int -> Bool -> Int")
  , ("renderType: parentheses on the left", renderType (TFun (TFun TInt TBool) TInt) == "(Int -> Bool) -> Int")
  , ("renderType: empty record", renderType (TRecord REmpty) == "{}")
  , ("renderType: closed record", renderType (TRecord (RExtend "a" TInt (RExtend "b" TBool REmpty))) == "{a : Int, b : Bool}")
  , ("renderType: open record", renderType (TRecord (RExtend "a" (TVar v0) (RVar v1))) == "{a : a | b}")
  , ("renderType: only a tail", renderType (TRecord (RVar v1)) == "{ | a}" )
  , ("renderType: a function in a field", renderType (TRecord (RExtend "f" (TFun TInt TInt) REmpty)) == "{f : Int -> Int}")
  , ("renderError: unbound", renderError (Unbound "x") == "unbound name: x")
  , ("renderError: mismatch", renderError (Unify (Mismatch TInt TBool)) == "type mismatch: Int and Bool")
  , ("renderError: infinite", renderError (Unify (Infinite v0 (TFun (TVar v0) (TVar v1)))) == "infinite type: a and a -> b")
  , ("renderError: both types share letters", renderError (Unify (Infinite v0 (TFun (TVar v1) (TVar v0)))) == "infinite type: a and b -> a")
  , ("renderError: infinite row", "infinite" `isInfixOf` renderError (Unify (InfiniteRow v0 REmpty)))
  , ("renderError: no field", renderError (Unify (MissingField "nmae" REmpty)) == "no field: nmae")
  , ("step: a value", session ["1"] == ["1"])
  , ("step: blank shows nothing", session [""] == [""])
  , ("step: a builtin", session ["add 1 2"] == ["3"])
  , ("step: a function", session ["x -> x"] == ["<function>"])
  , ("step: a record", session ["{a = 1}"] == ["{a = 1}"])
  , ("step: quit", session [":q", "1"] == ["<quit>"])
  , ("step: a binding shows its type", session ["x = 1"] == ["x : Int"])
  , ("step: a binding persists", final ["x = 1", "x"] == "1")
  , ("step: a binding can be replaced", final ["x = 1", "x = true", "x"] == "true")
  , ("step: a replacement changes the type", final ["x = 1", "x = true", "not x"] == "false")
  , ("step: a binding uses earlier bindings", final ["x = 1", "y = add x 1", "y"] == "2")
  , ("step: a binding stays polymorphic", final ["i = x -> x", "(a b -> a) (i 1) (i true)"] == "1")
  , ("step: a polymorphic binding shows its type", session ["i = x -> x"] == ["i : a -> a"])
  , ("step: a binding captures earlier names", final ["x = 1", "f = y -> x", "x = true", "f 0"] == "1")
  , ("step: a builtin can be replaced", final ["add = 1", "add"] == "1")
  , ("step: let does not leak", take 1 (reverse (session ["let x = 1 in x", "x"])) == ["command not found: x"])
  , ("step: a type query", ty "true" "Bool")
  , ("step: a literal is any Num type", ty "1" "Num a => a")
  , ("step: a type query for a polymorphic function", ty "x -> x" "a -> a")
  , ("step: a type query for an accessor", ty ".size" "{size : a | b} -> a")
  , ("step: a type query sees bindings", final ["x = 1", ":t x"] == "Int")
  , ("step: a type query runs nothing and binds nothing", final [":t y -> y", "y"] == "command not found: y")
  , ("step: a checker error", session ["not \"s\""] == ["type mismatch: Bool and String"])
  , ("step: a literal where Bool is wanted", session ["not 1"] == ["no instance: Num Bool"])
  , ("step: a parse error shows something", not (null (final ["(1"])))
  , ("step: a failed binding binds nothing", final ["x = not 1", "x"] == "command not found: x")
  , ("step: a failed binding keeps the old value", final ["x = 1", "x = not 1", "x"] == "1")
  , ("step: errors do not end a session", final ["nope", "1"] == "1")
  , ("command: a run binding", parsed names "x <- y" == Right (BindRun "x" (EVar "y")))
  , ("renderType: a computation", renderType ((TComp noEffects) TInt) == "Comp Int")
  , ("renderType: unit", renderType ((TComp noEffects) TUnit) == "Comp ()")
  , ("renderType: a computation of a function", renderType ((TComp noEffects) (TFun TInt TInt)) == "Comp (Int -> Int)")
  , ("renderType: a computation of a computation", renderType ((TComp noEffects) ((TComp noEffects) TInt)) == "Comp (Comp Int)")
  , ("renderType: a computation of a record", renderType ((TComp noEffects) (TRecord REmpty)) == "Comp {}")
  , ("renderType: a function that gives a computation", renderType (TFun TInt ((TComp noEffects) TInt)) == "Int -> Comp Int")
  , ("comp: print shows through stdout", prints ["print 1"] == ("1\n", [""]))
  , ("comp: pure shows its result", prints ["pure 1"] == ("", ["1"]))
  , ("comp: a unit result shows nothing", prints ["pure ()"] == ("", [""]))
  , ("comp: unit as a value shows", prints ["()"] == ("", ["()"]))
  , ("comp: = runs nothing", prints ["gs = print 1"] == ("", ["gs : Comp ()"]))
  , ("comp: each bare line runs it", prints ["gs = print 1", "gs", "gs"] == ("1\n1\n", ["gs : Comp ()", "", ""]))
  , ("comp: a type query runs nothing", prints [":t print 1"] == ("", ["Comp {console} ()"]))
  , ("comp: a function is not a computation", prints ["f = x -> print x"] == ("", ["f : a -> Comp ()"]))
  , ("comp: applying a function runs nothing", prints ["f = x -> print x", "g = f 1"] == ("", ["f : a -> Comp ()", "g : Comp ()"]))
  , ("comp: then a bare line runs it", fst (prints ["f = x -> print x", "g = f 1", "g"]) == "1\n")
  , ("comp: <- runs it", prints ["x <- print 1"] == ("1\n", ["x : ()"]))
  , ("comp: <- binds the result", final ["x <- pure 1", "add x 1"] == "2")
  , ("comp: <- runs once", fst (prints ["x <- print 1", "x", "x"]) == "1\n")
  , ("comp: <- refuses a value", final ["x <- true"] == "type mismatch: Bool and Comp a")
  , ("comp: a refused <- binds nothing", final ["x <- 1", "x"] == "command not found: x")
  , ("comp: a failed <- runs nothing", prints ["x <- print (not \"s\")"] == ("", ["type mismatch: Bool and String"]))
  , ("comp: only the outer computation runs", prints ["pure (print 1)"] == ("", ["<computation>"]))
  , ("comp: an inner computation can run later", fst (prints ["c <- pure (print 1)", "c"]) == "1\n")
  , ("comp: a run result stays polymorphic", final ["i <- pure (x -> x)", "(a b -> a) (i 1) (i true)"] == "1")
  , ("comp: a computation in a record runs by selection", fst (prints ["r = {a = print 1}", "r", "r.a"]) == "1\n")
  , ("comp: a computation prints on each run", fst (prints ["gs = print (add 1 2)", "gs", "gs"]) == "3\n3\n")
  , ("external: a command is a computation", ty "git status" "Stream {exec git} Bytes Bytes Status")
  , ("external: words are strings", parsed names "git log --oneline" == Right (Run (ECmd "git" [AWord "log", AWord "--oneline"])))
  , ("external: a string is one word", parsed names "echo \"a b\"" == Right (Run (ECmd "echo" [AWord "a b"])))
  , ("external: $x is a variable", parsed names "echo $x" == Right (Run (ECmd "echo" [AVar "x"])))
  , ("external: a dot in a word", parsed names "cat .bashrc" == Right (Run (ECmd "cat" [AWord ".bashrc"])))
  , ("external: a path head", parsed names "./run a" == Right (Run (ECmd "./run" [AWord "a"])))
  , ("external: an absolute head", parsed names "/bin/sh" == Right (Run (ECmd "/bin/sh" [])))
  , ("external: a bound head is a function", parsed names "f y" == Right (Run (EApp (EVar "f") (EVar "y"))))
  , ("external: a bound select stays a select", parsed names "x.a" == Right (Run (ESelect (EVar "x") "a")))
  , ("external: a lambda parameter is bound", parsed names "g = h -> h 1" == Right (Bind "g" (ELam "h" (EApp (EVar "h") (ELit (LInt 1))))))
  , ("external: a let name is bound in its body", parsed names "let h = 1 in h" == Right (Run (ELet "h" (ELit (LInt 1)) (EVar "h"))))
  , ("external: not bound past its scope", parsed names "(h -> h) (h)" == Right (Run (EApp (ELam "h" (EVar "h")) (ECmd "h" []))))
  , ("external: inside a lambda", parsed names "g = a -> echo $a" == Right (Bind "g" (ELam "a" (ECmd "echo" [AVar "a"]))))
  , ("external: ends at a bracket", parsed names "(echo a) " == Right (Run (ECmd "echo" [AWord "a"])))
  , ("external: an unbound argument is still an error", final ["not nope"] == "unbound name: nope")
  , ("external: output reaches stdout", prints ["echo hello"] == ("hello\n", [""]))
  , ("external: arguments arrive", fst (prints ["echo \"a b\" --c"]) == "a b --c\n")
  , ("external: a variable arrives", fst (prints ["x = \"hi\"", "echo $x"]) == "hi\n")
  , ("external: a variable must be a string", final ["x = 1", "echo $x"] == "type mismatch: Int and String")
  , ("external: a variable must exist", final ["echo $nope"] == "unbound name: nope")
  , ("external: success shows nothing", session ["sh -c \"exit 0\""] == [""])
  , ("external: failure shows its status", session ["sh -c \"exit 1\""] == ["exit 1"])
  , ("external: reserved words stay values", session ["true"] == ["true"])
  , ("external: a chosen status", session ["sh -c \"exit 3\""] == ["exit 3"])
  , ("external: a signal", session ["sh -c \"kill -9 $$\""] == ["signal 9"])
  , ("external: = runs nothing", prints ["g = echo hi"] == ("", ["g : Stream Bytes Bytes Status"]))
  , ("external: then each line runs it", fst (prints ["g = echo hi", "g", "g"]) == "hi\nhi\n")
  , ("external: <- binds the status", final ["s <- sh -c \"exit 1\"", "s"] == "exit 1")
  , ("external: through a function", fst (prints ["g = a -> echo $a", "g \"hi\""]) == "hi\n")
  , ("external: a bound name wins over a program", final ["echo = 1", "echo"] == "1")
  , ("external: an absolute path runs", session ["/bin/sh -c \"exit 4\""] == ["exit 4"])
  , ("preflight: a missing command", session ["nope-xyz"] == ["command not found: nope-xyz"])
  , ("preflight: on <- too", final ["s <- nope-xyz", "s"] == "command not found: s")
  , ("preflight: an unused latent action does not run", prints ["(a b -> a) (echo hi) (nope-xyz)"] == ("hi\n", [""]))
  , ("preflight: = does not check", session ["g = nope-xyz"] == ["g : Stream Bytes Bytes Status"])
  , ("preflight: effects survive a binding", final ["g = nope-xyz", "g"] == "command not found: nope-xyz")
  , ("string: a value", session ["\"a\""] == ["\"a\""])
  , ("string: print shows it plain", fst (prints ["print \"a\""]) == "a\n")
  , ("env: an overlay is a value", session [devLine] == ["dev : Overlay"])
  , ("env: it shows as its own source", final [devLine, "dev"] == "overlay \"dev\" [set \"LSH_A\" \"1\", set \"LSH_B\" \"x\"]")
  , ("env: use is a computation with the env effect", ty "use" "Overlay -> Comp {env} ()")
  , ("env: unuse takes a name", ty "unuse" "String -> Comp {env} ()")
  , ("env: environments", ty "environments" "Comp {env} [String]")
  , ("env: a patch", ty "prepend \"PATH\" \"/x\"" "Patch")
  , ("env: = does not activate", promptAfter [devLine] == "λ> ")
  , ("env: use activates", promptAfter [devLine, "use dev"] == "λ [dev]> ")
  , ("env: use shows nothing", session [devLine, "use dev"] == ["dev : Overlay", ""])
  , ("env: a program sees it", fst (prints [devLine, "use dev", "printenv LSH_A"]) == "1\n")
  , ("env: the process environment does not change", fst (prints [devLine, "use dev", "print 1"]) == "1\n" && unsafePerformIO (lookupEnv "LSH_A") == Nothing)
  , ("env: unuse removes it", fst (prints [devLine, "use dev", "unuse \"dev\"", "printenv LSH_A"]) == "")
  , ("env: unuse clears the prompt", promptAfter [devLine, "use dev", "unuse \"dev\""] == "λ> ")
  , ("env: a failed unuse reports", final ["unuse \"nope\""] == "no active environment: nope")
  , ("env: a failed unuse changes nothing", promptAfter [devLine, "use dev", "unuse \"nope\""] == "λ [dev]> ")
  , ("env: <- commits too", promptAfter [devLine, "u <- use dev"] == "λ [dev]> ")
  , ("env: a later overlay wins", fst (prints [devLine, "use dev", "use (overlay \"b\" [set \"LSH_A\" \"2\"])", "printenv LSH_A"]) == "2\n")
  , ("env: shadowing warns once", session [devLine, "use dev", "use (overlay \"b\" [set \"LSH_A\" \"2\"])", "pure ()"] == ["dev : Overlay", "", "warning: b shadows LSH_A from dev", ""])
  , ("env: shadowing shows in the prompt", promptAfter [devLine, "use dev", "use (overlay \"b\" [set \"LSH_A\" \"2\"])"] == "λ [dev b ⚠ LSH_A]> ")
  , ("env: the same value does not warn", session [devLine, "use dev", "use (overlay \"b\" [set \"LSH_A\" \"1\"])"] == ["dev : Overlay", "", ""])
  , ("env: unuse clears the flag", promptAfter [devLine, "use dev", "use (overlay \"b\" [set \"LSH_A\" \"2\"])", "unuse \"b\""] == "λ [dev]> ")
  , ("env: use again replaces, on top", final [devLine, "use dev", "use (overlay \"b\" [])", "use dev", "environments"] == "[\"b\", \"dev\"]")
  , ("env: prepend reaches PATH", fst (prints ["use (overlay \"p\" [prepend \"PATH\" \"/lsh-nowhere\"])", "sh -c \"printf %s \\\"$PATH\\\" | cut -d: -f1\""]) == "/lsh-nowhere\n")
  , ("env: the preflight resolves in the effective PATH", final ["use (overlay \"p\" [set \"PATH\" \"/lsh-nowhere\"])", "echo hi"] == "command not found: echo")
  , ("env: and unuse restores it", fst (prints ["use (overlay \"p\" [set \"PATH\" \"/lsh-nowhere\"])", "unuse \"p\"", "echo hi"]) == "hi\n")
  , ("env: unset hides a variable", fst (prints ["use (overlay \"u\" [unset \"HOME\"])", "sh -c \"printf %s ${HOME-unset}\""]) == "unset")
  , ("env: a pipeline sees it", final [devLine, "use dev", "x <- printenv LSH_B | lines | collect", "x.items"] == "[\"x\"]")
  , ("env: a value captured before use still sees the session when it runs", fst (prints ["g = printenv LSH_A", devLine, "use dev", "g"]) == "1\n")
  , ("do: a block is a computation", ty "do echo a; echo b end" "Comp {exec echo} Status")
  , ("do: effects are the union", ty "do print 1; echo a end" "Comp {exec echo, console} Status")
  , ("do: statements run in order", fst (prints ["do echo a; print 2; echo c end"]) == "a\n2\nc\n")
  , ("do: the value is the last result", final ["do pure 1; pure 2 end"] == "2")
  , ("do: <- binds for later statements", final ["do x <- pure 1; pure (add x 1) end"] == "2")
  , ("do: = names without running", prints ["do p = print 1; pure 3 end"] == ("", ["3"]))
  , ("do: a bound name is a function, not a command", fst (prints ["do x <- pure \"hi\"; echo $x end"]) == "hi\n")
  , ("do: a stream statement gives its status", final ["do sh -c \"exit 3\" end"] == "exit 3")
  , ("do: a failed command does not stop the block", fst (prints ["do sh -c \"exit 3\"; echo after end"]) == "after\n")
  , ("do: <- of a block", final ["r <- do s <- sh -c \"exit 4\"; pure s end", "r"] == "exit 4")
  , ("do: = runs nothing", prints ["b = do print 1 end"] == ("", ["b : Comp ()"]))
  , ("do: the preflight sees every statement", prints ["do echo a; nope-xyz end"] == ("", ["command not found: nope-xyz"]))
  , ("do: a trailing ; before end", final ["do pure 1; end"] == "1")
  , ("do: a quoted end is an argument", fst (prints ["do echo \"end\" end"]) == "end\n")
  , ("do: the last statement must be an expression", "must end with an expression" `isInfixOf` final ["do x <- pure 1 end"])
  , ("do: a value is not a statement", "type mismatch" `isInfixOf` final ["do true end"])
  , ("do: in a function", fst (prints ["g = x -> do echo $x; echo $x end", "g \"hi\""]) == "hi\nhi\n")
  , ("do: newlines between statements", fst (prints ["do print 1;\n print 2 end"]) == "1\n2\n")
  , ("with: the layer reaches the body", fst (prints [withLine, "with o do printenv LSH_W end"]) == "inside\n")
  , ("with: only for the block", fst (prints [withLine, "with o do print 1 end", "printenv LSH_W"]) == "1\n")
  , ("with: the prompt does not change", promptAfter [withLine, "with o do pure 1 end"] == "λ> ")
  , ("with: it wins over the session", fst (prints [withLine, "use (overlay \"s\" [set \"LSH_W\" \"session\"])", "with o do printenv LSH_W end", "printenv LSH_W"]) == "inside\nsession\n")
  , ("with: it discharges the commands it can see", ty "with (overlay \"o\" []) do ffmpeg-xyz end" "Comp Status")
  , ("with: so a missing command is found at spawn", final ["with (overlay \"o\" []) do nope-xyz end"] == "command not found: nope-xyz")
  , ("with: PATH from the layer resolves commands", final ["with (overlay \"p\" [set \"PATH\" \"/lsh-nowhere\"]) do echo hi end"] == "command not found: echo")
  , ("with: other effects stay", ty "with (overlay \"o\" []) do print 1 end" "Comp {console} ()")
  , ("with: a parameter's commands stay", ty "c -> with (overlay \"o\" []) do c end" "Comp { | a} b -> Comp { | a} b")
  , ("with: needs an overlay", "type mismatch" `isInfixOf` final ["with true do print 1 end"])
  , ("with: use inside it changes the session", promptAfter ["with (overlay \"o\" []) do use (overlay \"u\" []) end"] == "λ [u]> ")
  , ("needsMore: an open block", needsMore "do print 1;" && needsMore "with o do" && not (needsMore "do print 1 end"))
  , ("needsMore: strings and words", not (needsMore "echo \"do\"") && not (needsMore "echo done") && needsMore "x <- do")
  , ("ref: a reference is a String", ty "unuse nixpkgs#jq" "Comp {env} ()")
  , ("ref: use resolves with pkg", ty "use nixpkgs#jq" "Comp {exec nix, env} ()")
  , ("ref: with resolves with pkg", ty "with nixpkgs#ffmpeg do ffmpeg end" "Comp {exec nix} Status")
  , ("ref: dev takes one", ty "dev .#bench" "Comp {exec bash, exec nix} Overlay")
  , ("ref: an external argument is still a word", parsed names "nix build nixpkgs#jq" == Right (Run (ECmd "nix" [AWord "build", AWord "nixpkgs#jq"])))
  , ("ref: unuse by reference", final ["use (overlay \"nixpkgs#jq\" [])", "unuse nixpkgs#jq", "environments"] == "[]")
  , ("paths: ls takes a bareword", parsed sigs "ls src" == Right (Run (EApp (EVar "ls") (ELit (LStr "src")))))
  , ("paths: ls alone lists here", parsed sigs "ls" == Right (Run (EApp (EVar "ls") (ELit (LStr ".")))))
  , ("paths: ls before a pipe lists here", parsed sigs "ls | collect" == Right (Run (EPipe (EApp (EVar "ls") (ELit (LStr "."))) (EVar "collect"))))
  , ("paths: cd alone goes home", parsed sigs "cd" == Right (Run (EApp (EVar "cd") (ELit (LStr "~")))))
  , ("paths: .. and / are paths", parsed sigs "cd .." == Right (Run (EApp (EVar "cd") (ELit (LStr "..")))) && parsed sigs "cd /tmp" == Right (Run (EApp (EVar "cd") (ELit (LStr "/tmp")))))
  , ("paths: $x is a variable", parsed sigs "cd $d" == Right (Run (EApp (EVar "cd") (EVar "d"))))
  , ("paths: a string", parsed sigs "cd \"a b\"" == Right (Run (EApp (EVar "cd") (ELit (LStr "a b")))))
  , ("paths: parentheses hold an expression", parsed sigs "cd (f x)" == Right (Run (EApp (EVar "cd") (EApp (EVar "f") (EVar "x")))))
  , ("paths: a rebound builtin has no signature", parsed (BoundWith (Set.fromList ["ls", "f"]) Set.empty) "ls f" == Right (Run (EApp (EVar "ls") (EVar "f"))))
  , ("paths: a lambda parameter hides a signature", parsed sigs "ls -> ls f" == Right (Run (ELam "ls" (EApp (EVar "ls") (EVar "f")))))
  , ("paths: other builtins take expressions", parsed sigs "f .size" == Right (Run (EApp (EVar "f") (ELam "r" (ESelect (EVar "r") "size")))))
  , ("cd: a type", ty "x -> cd $x" "String -> Comp {cwd} ()")
  , ("cd: alone, it is applied to its default", ty "cd" "Comp {cwd} ()")
  , ("ls: a stream of records", ty "ls" "Stream {fs} Bytes {name : String, type : String, size : Int, ext : String} ()")
  , ("ls: a missing directory", final ["ls /lsh-nowhere"] == "ls: cannot read: /lsh-nowhere")
  , ("cd: a missing directory", final ["cd /lsh-nowhere"] == "cd: no such directory: /lsh-nowhere")
  , ("ls: entries are records", final ["r <- ls / | filter (e -> startsWith \"tmp\" e.name) | .name | collect", "r.items"] == "[\"tmp\"]")
  , ("useLocal: a type", ty "useLocal" "Overlay -> Comp {env} ()")
  , ("useLocal: active here", promptAfter ["useLocal (overlay \"l\" [])"] == "λ [l]> ")
  , ("allow: no flake here", take 1 (drop 1 (session ["cd /", "allow", "cd -"])) == ["no flake.nix here"])
  , ("bg: a job carries its row", ty "sleep 1 &" "Comp {exec sleep} (Job {exec sleep} Status)")
  , ("bg: a computation too", ty "pure 3 &" "Num a => Comp (Job a)")
  , ("bg: wait gives the result", ty "x -> wait x" "Job { | a} b -> Comp { | a} b")
  , ("bg: fg is the same shape", ty "x -> fg x" "Job { | a} b -> Comp { | a} b")
  , ("bg: a value is not a job", "type mismatch" `isInfixOf` final ["wait true"])
  , ("bg: & ends a word", parsed names "echo a&" == Right (Run (EBg (ECmd "echo" [AWord "a"]))))
  , ("bg: wait", final ["j <- pure 42 &", "wait j"] == "42")
  , ("bg: wait twice", drop 1 (session ["j <- sh -c \"exit 4\" &", "wait j", "wait j"]) == ["exit 4", "exit 4"])
  , ("bg: a block as a job", final ["j <- do x <- pure 1; pure (add x 1) end &", "wait j"] == "2")
  , ("bg: a failed job fails wait", final ["j <- nope-xyz &"] == "command not found: nope-xyz")
  , ("bg: the handle shows its number", final ["j <- pure 1 &", "j"] == "<job 1>")
  , ("bg: fg as a command still works", parsed (BoundWith (Set.fromList ["fg"]) (Set.fromList ["fg"])) "fg 2" == Right (Foreground (Just 2)))
  , ("bg: fg of an expression is the function", parsed (BoundWith (Set.fromList ["fg", "j"]) (Set.fromList ["fg"])) "fg j" == Right (Run (EApp (EVar "fg") (EVar "j"))))
  , ("class: a method's type", ty "eq" "Eq a => a -> a -> Bool" && ty "lt" "Ord a => a -> a -> Bool")
  , ("class: sort", ty "sort" "Ord a => [a] -> [a]")
  , ("class: a function that uses a method", ty "x -> eq x x" "Eq a => a -> Bool")
  , ("class: a superclass is provided", ty "x -> {a = eq x x, b = lt x x}" "Ord a => a -> {a : Bool, b : Bool}")
  , ("class: a concrete type needs no context", ty "eq true" "Bool -> Bool")
  , ("class: a literal adds Num", ty "eq 1" "(Eq a, Num a) => a -> Bool")
  , ("class: a list instance requires the element's", ty "x -> eq [x] [x]" "Eq a => a -> Bool")
  , ("class: an ambiguous type is not shown", ty "eq [] []" "Bool")
  , ("class: a binding generalises", session ["f = x -> eq x x"] == ["f : Eq a => a -> Bool"])
  , ("class: and is used at two types", final ["f = x -> eq x x", "{a = f 1, b = f \"s\"}"] == "{a = true, b = true}")
  , ("class: a let generalises", final ["let f = x -> eq x x in {a = f 1, b = f \"s\"}"] == "{a = true, b = true}")
  , ("class: a predicate on an outer variable waits for it", ty "y -> let f = x -> eq x y in f y" "Eq a => a -> Bool")
  , ("class: eq", final ["eq 1 1"] == "true" && final ["eq 1 2"] == "false")
  , ("class: ne", final ["ne \"a\" \"b\""] == "true")
  , ("class: lists compare elementwise", final ["eq [1, 2] [1, 2]"] == "true" && final ["eq [1, 2] [1]"] == "false")
  , ("class: lists order lexicographically", final ["lt [1, 2] [1, 3]"] == "true" && final ["lt [1] [1, 0]"] == "true" && final ["lt [2] [1, 9]"] == "false")
  , ("class: strings order", final ["lt \"apple\" \"banana\""] == "true")
  , ("class: le gt ge", final ["{a = le 1 1, b = gt 2 1, c = ge 1 2}"] == "{a = true, b = true, c = false}")
  , ("class: min and max", final ["{a = min 3 7, b = max 3 7}"] == "{a = 3, b = 7}")
  , ("class: sort", final ["sort [3, 1, 2]"] == "[1, 2, 3]" && final ["sort [[2], [1, 5], [1]]"] == "[[1], [1, 5], [2]]")
  , ("class: sortOn is stable", final ["sortOn (r -> r.n) [{n = 2, s = \"b\"}, {n = 1, s = \"a\"}, {n = 1, s = \"c\"}]"] == "[{n = 1, s = \"a\"}, {n = 1, s = \"c\"}, {n = 2, s = \"b\"}]")
  , ("class: statuses compare", final ["s <- sh -c \"exit 1\"", "{a = eq s s}"] == "{a = true}")
  , ("class: overlays compare", final ["eq (overlay \"o\" []) (overlay \"o\" [])"] == "true")
  , ("class: an ambiguous type defaults to ()", final ["eq [] []"] == "true")
  , ("class: a method in a pipeline", final ["r <- each [3, 1, 2] | filter (x -> lt x 3) | collect", "r.items"] == "[1, 2]")
  , ("class: a method in a block", fst (prints ["do x <- pure 3; print (lt x 4) end"]) == "true\n")
  , ("class: a method as a value", final ["m = (x -> x) lt", "m \"a\" \"b\""] == "type mismatch: () and String")
  , ("class: the restriction defaults a binding without parameters", session ["m = (x -> x) lt"] == ["m : () -> () -> Bool"])
  , ("class: a binding with parameters keeps its context", session ["m = x y -> lt x y"] == ["m : Ord a => a -> a -> Bool"])
  , ("num: a binding without parameters is one type", session ["x = 1"] == ["x : Int"] && session ["xs = [1, 2]"] == ["xs : [Int]"])
  , ("num: a let without parameters takes its type from use", final ["let n = 1 in add n 2"] == "3")
  , ("num: a result of <- defaults", session ["r <- pure 3"] == ["r : Int"])
  , ("class: functions have no Eq", final ["eq (x -> x) (x -> x)"] == "no instance: Eq (a -> a)")
  , ("record: equal", final ["eq {a = 1, b = \"x\"} {a = 1, b = \"x\"}"] == "true")
  , ("record: a field differs", final ["eq {a = 1, b = \"x\"} {a = 1, b = \"y\"}"] == "false")
  , ("record: field order does not matter", final ["eq {a = [1], b = {c = true}} {b = {c = true}, a = [1]}"] == "true")
  , ("record: the empty record", final ["eq {} {}"] == "true")
  , ("record: a field without Eq", final ["eq {f = x -> x} {f = x -> x}"] == "no instance: Eq (a -> a)")
  , ("record: no Ord", final ["lt {a = true} {a = false}"] == "no instance: Ord {a : Bool}")
  , ("record: an open row is a context", ty "r -> {x = r.name, y = eq r r}" "(Eq a, Eq { | b}) => {name : a | b} -> {x : a, y : Bool}")
  , ("record: and the caller supplies the rest", final ["f = r -> {x = r.name, y = eq r r}", "f {name = \"n\", size = 3, tags = [\"t\"]}"] == "{x = \"n\", y = true}")
  , ("record: the rest is compared too", final ["f = r s -> {x = r.name, y = eq r s}", "f {name = \"n\", size = 3} {name = \"n\", size = 4}"] == "{x = \"n\", y = false}")
  , ("record: ls entries", final ["r <- ls / | filter (e -> eq {t = e.type, n = e.name} {t = \"dir\", n = \"usr\"}) | .name | collect", "r.items"] == "[\"usr\"]")
  , ("record: :elab shows the row evidence", let code = final [":elab f = r -> {x = r.name, y = eq r r}"]
      in all (`isInfixOf` code) ["(%instance Eq Record) (%extendFields {name = %d", ".%fields)"])
  , ("class: a computation has no Eq", final ["eq (pure true) (pure true)"] == "no instance: Eq (Comp Bool)")
  , ("class: Bytes have no Ord", final ["lt (encodeUtf8 \"a\") (encodeUtf8 \"b\")"] == "no instance: Ord Bytes")
  , ("class: a rebound method is an ordinary function", final ["eq = x -> x", "eq 1"] == "1")
  , ("class: evidence comes before what uses it", final ["p = x -> {a = eq [x] [x], b = lt x x}", "p 1"] == "{a = true, b = false}")
  , ("class: a defaulted element dictionary is bound first", final ["eq [[]] [[]]"] == "true")
  , (":elab: a dictionary", session [":elab eq true false"] == ["let %d5 = (%instance Eq Bool) in eq %d5 true false"])
  , (":elab: a literal is fromInt", ("%fromInt %d" `isInfixOf`) (final [":elab print 1"]))
  , (":elab: an instance needs its argument's, first", session [":elab eq [] []"] == ["let %d10 = (%instance Eq ()) in let %d5 = (%instance Eq List) %d10 in eq %d5 [] []"])
  , (":elab: a binding takes its dictionaries", session [":elab f = x -> eq x x"] == ["f = %d6 x -> eq %d6 x x"])
  , (":elab: a superclass is a field", ("let %d6 = %d10.%Eq" `isInfixOf`) (final [":elab p = x -> {a = eq x x, b = lt x x}"]))
  , (":elab: no classes, no change", session [":elab print true"] == ["print true"])
  , (":elab: runs nothing", prints [":elab print true"] == ("", ["print true"]))
  , (":elab: errors as the line would", final [":elab eq (x -> x) (x -> x)"] == "no instance: Eq (a -> a)")
  , (":core: a lambda", session [":core x y -> x"] == ["x y -> x"])
  , (":core: an accessor", session [":core .a.b"] == ["r -> r.a.b"])
  , (":core: a block", session [":core do x <- pure 1; y = add x 1; print y end"] == ["bind x <- pure 1; let y = add x 1 in run (print y)"])
  , (":core: a dropped result", session [":core do echo a; echo b end"] == ["bind %1 <- echo a; run (echo b)"])
  , (":core: a bare reference", session [":core use nixpkgs#jq"] == ["bind %1 <- %pkg \"nixpkgs#jq\"; run (%use %1)"])
  , (":core: a path slot's default", session [":core ls | collect"] == ["ls \".\" | collect"])
  , (":core: a job", session [":core sleep 1 &"] == ["bg (sleep 1)"])
  , (":core: arguments that need quotes", session [":core echo \"a b\" $x"] == ["echo \"a b\" $x"])
  , (":core: runs nothing", prints [":core print 1"] == ("", ["print 1"]))
  , (":env: nothing active", final [":env"] == "no active environments")
  , (":env: lists patches", final [devLine, "use dev", ":env"] == "dev\n  set \"LSH_A\" \"1\"\n  set \"LSH_B\" \"x\"")
  , (":env: marks shadowing", ("⚠ shadows dev" `isInfixOf`) (final [devLine, "use dev", "use (overlay \"b\" [set \"LSH_A\" \"2\"])", ":env"]))
  , (":env: a variable and its layers", final [devLine, "use dev", "use (overlay \"b\" [set \"LSH_A\" \"2\"])", ":env LSH_A"] == "LSH_A = \"2\"\n  base  unset\n  dev  set \"LSH_A\" \"1\"\n  b  set \"LSH_A\" \"2\"")
  , (":env: an unset variable", final [":env LSH_NOPE"] == "LSH_NOPE is unset\n  base  unset")
  , (":env: a prepend shows the base", ("/lsh-x:" `isInfixOf`) (final ["use (overlay \"p\" [prepend \"PATH\" \"/lsh-x\"])", ":env PATH"]))
  , ("op: arithmetic", final ["1 + 2 * 3 - 4 / 2"] == "5")
  , ("op: comparisons and logic", final ["1 < 2 && 2 <= 2 || false"] == "true")
  , ("op: a chained comparison is an error", ("cannot mix == and ==" `isInfixOf`) (final ["1 == 2 == 3"]))
  , ("op: <>", final ["\"ab\" <> \"cd\""] == "\"abcd\"" && final ["[1] <> [2, 3]"] == "[1, 2, 3]")
  , ("op: minus by spacing", session ["x = 10", "x-1", "x - 1", "(- 1) x", "(x -) 1"] == ["x : Int", "9", "9", "9", "9"])
  , ("op: composition", final ["f = (+ 1) >>> (* 2)", "f 3"] == "8" && final ["((+ 1) <<< (* 2)) 3"] == "7")
  , ("op: an accessor section", final ["each [{size = 1}, {size = 3}] | filter (.size > 1) | collect"] == "{items = [{size = 3}], status = (), statuses = []}")
  , ("op: the type of a section", ty "(== 1)" "(Eq a, Num a) => a -> Bool")
  , ("op: define an operator", session ["(<+>) = a b -> a * 10 + b", "1 <+> 2"] == ["(<+>) : Num a => a -> a -> a", "12"])
  , ("op: a user operator is infixl 9", final ["(<+>) = a b -> a * 10 + b", "1 <+> 2 + 3"] == "15")
  , ("op: a fixity line lasts for the session", session ["(<+>) = a b -> a * 10 + b", "infixl 6 <+>", "1 <+> 2 + 3", "1 <+> 2 * 3"] == ["(<+>) : Num a => a -> a -> a", "", "15", "16"])
  , ("op: a non-associative fixity", ("cannot mix ~= and ~=" `isInfixOf`) (final ["(~=) = eq", "infix 4 ~=", "1 ~= 2 ~= 3"]))
  , ("op: infix is still a name", final ["infix = x -> x", "infix 3"] == "3")
  , (":core: operators read infix", session [":core 1 + 2 * 3", ":core (.size > 1)", ":core (+)"] == ["1 + (2 * 3)", "%1 -> %1.size > 1", "(+)"])
  , (":elab: an operator with its dictionary reads as a function", ("(+) %d" `isInfixOf`) (final [":elab 1 + 2"]))
  , ("class: declare and use", session [prettyClass, ":t pretty", prettyInt, "pretty 3"] == ["class Pretty", "Pretty a => a -> String", "instance Pretty Int", "\"#3\""])
  , ("class: no instance yet", final [prettyClass, "pretty true"] == "no instance: Pretty Bool")
  , ("class: a binding abstracts over it", final [prettyClass, "f = x -> pretty x"] == "f : Pretty a => a -> String")
  , ("class: an instance with a context", session [prettyClass, prettyInt, prettyList, "pretty [1, 2]", "pretty [true]"]
      == ["class Pretty", "instance Pretty Int", "instance Pretty [a]", "\"2 items\"", "no instance: Pretty Bool"])
  , ("class: nested instances", final [sizeClass, "instance Size Int where size = n -> 1", "instance Size a => Size [a] where size = xs -> length xs + 100", "size [[1, 2], [3]]"] == "102")
  , ("class: several methods in a block", final ["class Shape a where do area : a -> Int; name : a -> String end", "instance Shape Int where do area = n -> n * n; name = n -> \"sq\" end", "{a = area 3, n = name 3}"] == "{a = 9, n = \"sq\"}")
  , ("class: a multi-line block", needsMore "class Shape a where do" && not (needsMore "class Shape a where do area : a -> Int end"))
  , ("class: a function head", final ["class Tag a where tag : a -> String", "instance Tag (a -> b) where tag = f -> \"fn\"", "tag (x -> x)"] == "\"fn\"")
  , ("class: an operator method", final ["class Join a where (<+>) : a -> a -> a", "instance Join Int where (<+>) = a b -> a * 10 + b", "1 <+> 2"] == "12")
  , ("class: a record method type", final ["class Box a where open : {value : a | r} -> a", "instance Box Int where open = r -> r.value", "open {value = 4, extra = true}"] == "4")
  , ("superclass: provides the superclass", ty' [hashClass, hashInt] "x -> {h = hash x, e = x == x}" "Hashable a => a -> {h : Int, e : Bool}")
  , ("superclass: an instance needs it", final [hashClass, "instance Hashable (a -> b) where hash = f -> 1"] == "no instance: Eq (a -> b)")
  , ("superclass: a context gives it", final [hashClass, hashInt, "instance Hashable a => Hashable [a] where hash = xs -> length xs", "hash [[1]]"] == "1")
  , ("superclass: a context too weak", final ["class Ord a => Rank a where rank : a -> Int", "instance Rank [a] where rank = xs -> 1"] == "the instance needs Ord a, and its context does not give it")
  , ("superclass: must be a class", final ["class Nope a => Rank a where rank : a -> Int"] == "no class Nope")
  , ("instance: replaces for later lines", session [prettyClass, prettyInt, "instance Pretty Int where pretty = n -> \"new\"", "pretty 1"] == ["class Pretty", "instance Pretty Int", "instance Pretty Int", "\"new\""])
  , ("instance: a prelude instance too", final ["instance Show Int where show = n -> \"int\"", "[1, 2]"] == "[int, int]")
  , ("instance: a missing method", final ["class Shape a where do area : a -> Int; name : a -> String end", "instance Shape Int where area = n -> n"] == "the instance has no name")
  , ("instance: an extra method", final ["instance Show Bool where do show = b -> \"B\"; extra = 1 end"] == "extra is not a method of Show")
  , ("instance: a wrong method type", final [prettyClass, "instance Pretty Int where pretty = n -> n"] == "type mismatch: Int and String")
  , ("instance: less general than the class", final ["class Container f where empty : f -> [f]", "instance Container [a] where empty = xs -> [[1]]"] == "the instance needs Num a, and its context does not give it")
  , ("instance: a rigid head", ("less general" `isInfixOf`) (final ["class C a where c : a -> a", "instance C [a] where c = xs -> [1] <> xs <> [true]"]) || ("mismatch" `isInfixOf`) (final ["class C a where c : a -> a", "instance C [a] where c = xs -> [1] <> xs <> [true]"]))
  , ("instance: distinct unknowns in the head", final [prettyClass, "instance Pretty [Int] where pretty = xs -> \"\""] == "an instance's head takes distinct unknowns: [a], not [Int]")
  , ("instance: no such class", final ["instance Nope Int where x = 1"] == "no class Nope")
  , ("instance: a context on the head", final [prettyClass, "instance Pretty b => Pretty [a] where pretty = xs -> \"\""] == "a context constrains the head's unknowns")
  , ("instance: it may use itself", final [prettyClass, prettyInt, "instance Pretty a => Pretty [a] where pretty = xs -> \"[\" <> pretty (length xs) <> \"]\"", "pretty [[1]]"] == "\"[#1]\"")
  , ("class: a prelude class stays", final ["class Eq a where foo : a -> a"] == "Eq is a prelude class")
  , ("class: a method mentions the class's unknown", final ["class Bad a where bad : Int"] == "bad must mention a")
  , ("class: redeclaring drops its instances", final [prettyClass, prettyInt, "class Pretty a where pretty : a -> Int", "pretty (length [])"] == "no instance: Pretty Int")
  , ("show: values print through Show", session ["instance Show Int where show = n -> \"int\"", "1", "{a = 1, b = [true]}", "pure 3", "x -> x"] == ["instance Show Int", "int", "{a = int, b = [true]}", "int", "<function>"])
  , ("show: as before for the prelude", session ["1", "\"s\"", "[[1], []]", "{a = (), b = false}"] == ["1", "\"s\"", "[[1], []]", "{a = (), b = false}"])
  , ("show: the method", final ["show [1, 2] <> show true"] == "\"[1, 2]true\"")
  , ("show: no instance for a function", final ["show (x -> x)"] == "no instance: Show (a -> a)")
  , (":elab: an instance's dictionary", ("(%instance Show List) = %d" `isInfixOf`) (final [":elab instance Show a => Show [a] where show = xs -> \"n\""]))
  , (":elab: a superclass field", ("%Eq = " `isInfixOf`) (final [hashClass, ":elab " <> hashInt]))
  , ("default: a method left out takes it", session ["class Pretty a where do pretty : a -> String; prettyAll : [a] -> String; prettyAll = xs -> \"many\" end", "instance Pretty Int where pretty = n -> \"#\"", "prettyAll [1]"] == ["class Pretty", "instance Pretty Int", "\"many\""])
  , ("default: an instance may still define it", final ["class Pretty a where do pretty : a -> String; prettyAll : [a] -> String; prettyAll = xs -> \"many\" end", "instance Pretty Bool where do pretty = b -> \"b\"; prettyAll = xs -> \"bools\" end", "prettyAll [true]"] == "\"bools\"")
  , ("default: it uses the instance's other methods", final ["class Size a where do size : a -> Int; big : a -> Bool; big = x -> size x > 10 end", "instance Size Int where size = n -> n", "{a = big 20, b = big 2}"] == "{a = true, b = false}")
  , ("default: checked against its signature", final ["class Q a where do q : a -> Int; q = x -> \"s\" end"] == "type mismatch: Int and String")
  , ("default: needs a signature", final ["class R a where do r : a -> Int; s = x -> 1 end"] == "s has a default but no signature")
  , ("default: Eq needs only eq", final ["instance Eq Bool where eq = a b -> true", "true != false"] == "false")
  , ("default: Ord needs only lt", final ["instance Ord Bool where lt = a b -> false", "{m = min true false, l = true <= false, g = true > false}"] == "{m = true, l = true, g = false}")
  , ("default: the prelude's are its own", final ["not = x -> x", "instance Eq Bool where eq = a b -> true", "true != false"] == "false")
  , ("init: bindings and declarations, quietly", scripted "x = 41\nclass P a where p : a -> Int\ninstance P Int where p = n -> n + x\ninfixl 6 <+>\n(<+>) = a b -> a * 10 + b\n" ["p 1", "1 <+> 2 * 3"] == ([], ["42", "16"]))
  , ("init: errors name their line, and the rest still runs", scripted "x = 1\ny = x + true\nz = 3\n" ["z"] == ([":2: type mismatch: Int and Bool"], ["3"]))
  , ("init: a block spans lines", scripted "f = x -> do\n  print x;\n  pure x\nend\n" [":t f"] == ([], ["a -> Comp {console} a"]))
  , ("init: a value shows", fst (scripted "1 + 1\n" []) == ["2"])
  , ("init: :q ends the file", ("exec y" `isInfixOf`) (last (snd (scripted "x = 1\n:q\ny = 2\n" [":t y"]))))
  , ("init: a missing file is none", unsafePerformIO (snd <$> runScript "/lsh-no-such/init.lsh" initial) == [])
  , ("init: comments and blank lines", scripted "-- setup\n\nx = 2 -- two\n" ["x"] == ([], ["2"]))
  , ("annotate: narrows a type", session ["(1 : Int)", ":t (x -> x : Int -> Int)", ":t ([] : [String])"] == ["1", "Int -> Int", "[String]"])
  , ("annotate: must fit", final ["(true : Int)"] == "type mismatch: Bool and Int")
  , ("annotate: :core shows it", final [":core (1 : Int)"] == "(1 : Int)")
  , ("signature: decides the type", session ["g : Int -> Int", "g = x -> x", ":t g"] == ["", "g : Int -> Int", "Int -> Int"])
  , ("signature: a context", session ["f : Ord a => [a] -> [a]", "f = xs -> sort xs", "f [3, 1]"] == ["", "f : Ord a => [a] -> [a]", "[1, 3]"])
  , ("signature: too general", final ["h : a -> a", "h = x -> x + 1"] == "the binding needs Num a, and its signature does not give it")
  , ("signature: a superclass is enough", final ["q : Ord a => a -> Bool", "q = x -> x == x", "q 1"] == "true")
  , ("signature: a superclass is not a subclass", final ["p : Eq a => a -> Bool", "p = x -> x < x"] == "the binding needs Ord a, and its signature does not give it")
  , ("signature: rigid unknowns", final ["h : a -> b", "h = x -> x"] == "the binding is less general than its signature")
  , ("signature: lifts the restriction", session ["n : Num a => a", "n = 1", "{a = (n : Int), b = n}"] == ["", "n : Num a => a", "{a = 1, b = 1}"])
  , ("signature: what it leaves open defaults", final ["r : b -> Bool", "r = x -> eq [] []", "r 1"] == "true")
  , ("signature: a record tail is flexible", final ["s : {name : String | r} -> String", "s = x -> x.name", "s {name = \"n\", size = 1}"] == "\"n\"")
  , ("signature: rows reach the preflight", session ["c : Int -> Comp ()", "c = n -> print n", ":t c"] == ["", "c : Int -> Comp ()", "Int -> Comp {console} ()"])
  , ("signature: waits for its own name", session ["z : Int", "y = 2", "z = 3", ":t z"] == ["", "y : Int", "z : Int", "Int"])
  , ("signature: used up by its binding", final ["g : Int -> Int", "g = x -> x", "g = x -> true"] == "g : a -> Bool")
  , ("signature: a failed binding keeps it", final ["g : Int -> Int", "g = x -> true", "g = x -> x"] == "g : Int -> Int")
  , ("signature: a class it uses", final [prettyClass, "f : Pretty a => a -> String", "f = x -> pretty x <> \"!\""] == "f : Pretty a => a -> String")
  , ("signature: no such class", final ["r : Nope a => a"] == "no class Nope")
  , ("signature: a context on its own unknowns", final ["r : Eq b => a -> a"] == "a signature's context constrains its own unknowns")
  , ("init: a signature is quiet", fst (scripted "g : Int -> Int\ng = x -> x\n" []) == [])
  , ("rows: written as printed", session ["c : Int -> Comp {console} ()", "c = n -> print n", ":t c"] == ["", "c : Int -> Comp ()", "Int -> Comp {console} ()"])
  , ("rows: a closed row is a ceiling", final ["c : Int -> Comp {console} ()", "c = n -> pure ()", ":t c"] == "Int -> Comp {console} ()")
  , ("rows: more than the ceiling", final ["c : Comp {} ()", "c = print 1"] == "effect mismatch: {console} and {}")
  , ("rows: an omitted row is open", final ["e : Int -> Comp ()", "e = n -> print n", ":t e"] == "Int -> Comp {console} ()")
  , ("rows: a tail ties rows together", session ["h : Comp {| e} a -> Comp {console | e} a", "h = m -> do print 1; m end", ":t h (pure 1)"] == ["", "h : Comp a -> Comp a", "Num a => Comp {console} a"])
  , ("rows: an annotation is a ceiling too", final [":t (pure 1 : Comp {console} Int)"] == "Comp {console} Int")
  , ("rows: an exec label", final ["k : Comp {exec git} Status", "k = do r <- git --version | collect; pure r.status end", ":t k"] == "Comp {exec git} Status")
  , ("rows: a method's row is its ceiling", session [greetClass, "instance Greet Int where greet = n -> print n", "instance Greet String where greet = s -> pure ()", ":t greet"]
      == ["class Greet", "instance Greet Int", "instance Greet String", "Greet a => a -> Comp {console} ()"])
  , ("rows: an instance may not exceed it", final [greetClass, "instance Greet Bool where greet = b -> do print 1; cd \"/\" end"] == "effect mismatch: {console, cwd} and {console}")
  , ("rows: an omitted row in a method is empty", final ["class Quiet a where quiet : a -> Comp ()", "instance Quiet Int where quiet = n -> print n"] == "effect mismatch: {console} and {}")
  , ("rows: the preflight knows a method's effects", final [greetClass, "instance Greet Int where greet = n -> print n", "greet 5"] == "")
  , ("help: an overview", ("topics  syntax" `isInfixOf`) (final [":help"]) && final [":h"] == final [":help"])
  , ("help: a builtin, with its type", final [":help map"] == "map : (a -> b) -> Stage a b\n  a stage that applies a function to each item")
  , ("help: an operator, with its fixity", final [":help <>"] == "(<>) : Semigroup a => a -> a -> a\n  combine: strings, lists, bytes, overlays (later patches win)\n  infixr 5")
  , ("help: a user operator", final ["(<+>) = a b -> a", "infixr 2 <+>", ":help <+>"] == "(<+>) : a -> b -> a\n  infixr 2")
  , ("help: a rebound name loses its description", final ["map = 1", ":help map"] == "map : Int")
  , ("help: a class", final [":help Ord"] == "class Eq a => Ord a\n  lt : a -> a -> Bool\n  le : a -> a -> Bool   (default)\n  gt : a -> a -> Bool   (default)\n  ge : a -> a -> Bool   (default)\n  min : a -> a -> a   (default)\n  max : a -> a -> a   (default)\ninstances  Int, Bool, String, (), [a] when Ord a")
  , ("help: a declared class", final [prettyClass, prettyInt, ":help Pretty"] == "class Pretty a\n  pretty : a -> String\ninstances  Int")
  , ("help: a topic", ("docs/jobs.md" `isInfixOf`) (final [":help jobs"]))
  , ("help: every topic has text", all (\t -> not (null (final [":help " <> t]))) ["syntax", "operators", "pipelines", "jobs", "environments", "nix", "classes", "types", "startup"])
  , ("help: every builtin is described", null [n | n <- builtinNames, "nothing named" `isInfixOf` final [":help " <> n] || length (lines (final [":help " <> n])) < 2])
  , ("help: nothing by that name", final [":help nope"] == "nothing named nope; :help lists the topics")
  , ("if: picks a branch", session ["if 1 < 2 then \"yes\" else \"no\"", "if false then 1 else 2"] == ["\"yes\"", "2"])
  , ("if: the condition is a Bool", final ["if 1 then 2 else 3"] == "no instance: Num Bool")
  , ("if: branches agree", final ["if true then 1 else \"s\""] == "no instance: Num String")
  , ("if: computations join their rows", ty "c -> if c then print 1 else pure ()" "Bool -> Comp {console} ()")
  , ("if: streams too", ty "c -> if c then echo hi else git status" "Bool -> Stream {exec echo, exec git} Bytes Bytes Status")
  , ("if: only the chosen branch runs", prints ["if true then print 1 else print 2"] == ("1\n", [""]))
  , ("if: in a block", prints ["do print 1; if false then print 2 else print 3; print 4 end"] == ("1\n3\n4\n", [""]))
  , ("if: recursion-free abs", final ["abs = n -> if n < 0 then negate n else n", "abs -5"] == "5")
  , ("if: :core", final [":core if a then b else c"] == "if a then b else c")
  , (":elab: declares nothing", ("exec pretty" `isInfixOf`) (final [":elab " <> prettyClass, ":t pretty"]))
  ]
 where
  builtinNames = words "print pure andThen collect map filter lines each pipefail length sort sortOn contains startsWith isEmpty stringLength byteLength encodeUtf8 encodeLine not add show eq ne lt le gt ge min max negate div mod cd ls overlay set unset prepend append prependWith appendWith use useLocal unuse environments pkg dev wait fg && || >>> <<< <> + - * / % == != < <= > >="
  greetClass = "class Greet a where greet : a -> Comp {console} ()"
  prettyClass = "class Pretty a where pretty : a -> String"
  prettyInt = "instance Pretty Int where pretty = n -> \"#\" <> show n"
  prettyList = "instance Pretty a => Pretty [a] where pretty = xs -> show (length xs) <> \" items\""
  sizeClass = "class Size a where size : a -> Int"
  hashClass = "class Eq a => Hashable a where hash : a -> Int"
  hashInt = "instance Hashable Int where hash = n -> n * 31"
  ty' setup input expected = last (session (setup <> [":t " <> input])) == expected
  withLine = "o = overlay \"o\" [set \"LSH_W\" \"inside\"]"
  devLine = "dev = overlay \"dev\" [set \"LSH_A\" \"1\", set \"LSH_B\" \"x\"]"

runTest :: (String, Bool) -> IO Bool
runTest (name, result) = do
  outcome <- try (timeout 2000000 (evaluate result)) :: IO (Either SomeException (Maybe Bool))
  let (passed, label) = case outcome of
        Right (Just True) -> (True, "pass")
        Right (Just False) -> (False, "FAIL")
        Left _ -> (False, "TODO")
        Right Nothing -> (False, "HANG")
  putStrLn (label <> "  " <> name)
  pure passed

main :: IO ()
main = do
  -- Unit tests must not touch the terminal they were started from: with a
  -- tty on stdin, jobs hand the terminal over and back, which can stop the
  -- whole test run in an interactive shell. With stdin on /dev/null that code
  -- is skipped, and a program that reads stdin sees end of input.
  devNull <- openFile "/dev/null" ReadMode
  hDuplicateTo devNull stdin
  -- Tests must not read or write the user's approvals: an approved flake in
  -- the directory the tests start in would otherwise activate, hook and all.
  temporary <- getTemporaryDirectory
  let scratch = temporary <> "/lambdash-repl-tests"
  exists <- doesDirectoryExist scratch
  when exists (removeDirectoryRecursive scratch)
  createDirectoryIfMissing True scratch
  setEnv "XDG_STATE_HOME" scratch
  -- Nor may they depend on the directory they start in: arriving back in a
  -- directory with a flake.nix runs nix to hash it, which can be slow. An
  -- empty directory has nothing to activate.
  let work = scratch <> "/work"
  createDirectoryIfMissing True work
  setCurrentDirectory work
  results <- mapM runTest tests
  putStrLn (show (length (filter id results)) <> "/" <> show (length results) <> " passed")
  unless (and results) exitFailure
