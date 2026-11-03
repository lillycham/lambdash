-- | REPL tests. Each runs lines through 'step', as the loop does, and compares
-- the text it would show.
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (unless, when)
import Data.List (isInfixOf)
import Data.Set qualified as Set
import Lambdash.Infer
import Lambdash.Parser (Bound (..))
import Lambdash.Repl
import Lambdash.Unify
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getCurrentDirectory, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.Exit (exitFailure)
import System.IO (hClose, hFlush, openTempFile, stdout)
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

tests :: [(String, Bool)]
tests =
  [ ("command: blank", parseCommand names "" == Right Blank && parseCommand names "  -- note" == Right Blank)
  , ("command: quit", parseCommand names ":q" == Right Exit)
  , ("command: an expression", parseCommand names "x" == Right (Run (EVar "x")))
  , ("command: an application is not a binding", parseCommand names "f x" == Right (Run (EApp (EVar "f") (EVar "x"))))
  , ("command: a binding", parseCommand names "x = 1" == Right (Bind "x" (ELit (LInt 1))))
  , ("command: a lambda binding", parseCommand names "i = x -> x" == Right (Bind "i" (ELam "x" (EVar "x"))))
  , ("command: a type query", parseCommand names ":t x" == Right (TypeOf (EVar "x")))
  , ("command: let is still an expression", parseCommand names "let x = 1 in x" == Right (Run (ELet "x" (ELit (LInt 1)) (EVar "x"))))
  , ("command: commands in a block", parseCommand names "do cabal build; cabal test end" == Right (Run (EDo [SRun (ECmd "cabal" [AWord "build"]), SRun (ECmd "cabal" [AWord "test"])])))
  , ("command: end ends a command line in a block", parseCommand names "do echo a end" == Right (Run (EDo [SRun (ECmd "echo" [AWord "a"])])))
  , ("command: end is a word outside a block", parseCommand names "echo end" == Right (Run (ECmd "echo" [AWord "end"])))
  , ("command: ; ends a bareword", parseCommand names "do echo a;echo b end" == Right (Run (EDo [SRun (ECmd "echo" [AWord "a"]), SRun (ECmd "echo" [AWord "b"])])))
  , ("command: a block binding is not a command", parseCommand names "do g <- f; g 1 end" == Right (Run (EDo [SBind "g" (EVar "f"), SRun (EApp (EVar "g") (ELit (LInt 1)))])))
  , ("command: jobs", parseCommand names "jobs" == Right ListJobs)
  , ("command: fg", parseCommand names "fg" == Right (Foreground Nothing))
  , ("command: fg with a number", parseCommand names "fg 2" == Right (Foreground (Just 2)) && parseCommand names "fg %2" == Right (Foreground (Just 2)))
  , ("command: a bound fg is a value", parseCommand (BoundNames (Set.fromList ["fg"])) "fg" == Right (Run (EVar "fg")))
  , ("command: fg with a word is a command line", parseCommand names "fg x" == Right (Run (ECmd "fg" [AWord "x"])))
  , ("step: fg with no jobs", session ["fg"] == ["no jobs"] && session ["fg 3"] == ["no job 3"])
  , ("step: jobs with no jobs", session ["jobs"] == [""])
  , ("command: bg", parseCommand names "bg" == Right (Background Nothing) && parseCommand names "bg %3" == Right (Background (Just 3)))
  , ("step: bg with no jobs", session ["bg"] == ["no stopped jobs"] && session ["bg 2"] == ["no stopped job 2"])
  , ("step: :q with no jobs quits at once", session [":q"] == ["<quit>"])
  , ("command: an unknown command", either (const True) (const False) (parseCommand names ":nope"))
  , ("command: a binding needs a value", either (const True) (const False) (parseCommand names "x ="))
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
  , ("step: a type query", ty "1" "Int")
  , ("step: a type query for a polymorphic function", ty "x -> x" "a -> a")
  , ("step: a type query for an accessor", ty ".size" "{size : a | b} -> a")
  , ("step: a type query sees bindings", final ["x = 1", ":t x"] == "Int")
  , ("step: a type query runs nothing and binds nothing", final [":t y -> y", "y"] == "command not found: y")
  , ("step: a checker error", session ["not 1"] == ["type mismatch: Bool and Int"])
  , ("step: a parse error shows something", not (null (final ["(1"])))
  , ("step: a failed binding binds nothing", final ["x = not 1", "x"] == "command not found: x")
  , ("step: a failed binding keeps the old value", final ["x = 1", "x = not 1", "x"] == "1")
  , ("step: errors do not end a session", final ["nope", "1"] == "1")
  , ("command: a run binding", parseCommand names "x <- y" == Right (BindRun "x" (EVar "y")))
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
  , ("comp: <- refuses a value", final ["x <- 1"] == "type mismatch: Int and Comp a")
  , ("comp: a refused <- binds nothing", final ["x <- 1", "x"] == "command not found: x")
  , ("comp: a failed <- runs nothing", prints ["x <- print (not 1)"] == ("", ["type mismatch: Bool and Int"]))
  , ("comp: only the outer computation runs", prints ["pure (print 1)"] == ("", ["<computation>"]))
  , ("comp: an inner computation can run later", fst (prints ["c <- pure (print 1)", "c"]) == "1\n")
  , ("comp: a run result stays polymorphic", final ["i <- pure (x -> x)", "(a b -> a) (i 1) (i true)"] == "1")
  , ("comp: a computation in a record runs by selection", fst (prints ["r = {a = print 1}", "r", "r.a"]) == "1\n")
  , ("comp: a computation prints on each run", fst (prints ["gs = print (add 1 2)", "gs", "gs"]) == "3\n3\n")
  , ("external: a command is a computation", ty "git status" "Stream {exec git} Bytes Bytes Status")
  , ("external: words are strings", parseCommand names "git log --oneline" == Right (Run (ECmd "git" [AWord "log", AWord "--oneline"])))
  , ("external: a string is one word", parseCommand names "echo \"a b\"" == Right (Run (ECmd "echo" [AWord "a b"])))
  , ("external: $x is a variable", parseCommand names "echo $x" == Right (Run (ECmd "echo" [AVar "x"])))
  , ("external: a dot in a word", parseCommand names "cat .bashrc" == Right (Run (ECmd "cat" [AWord ".bashrc"])))
  , ("external: a path head", parseCommand names "./run a" == Right (Run (ECmd "./run" [AWord "a"])))
  , ("external: an absolute head", parseCommand names "/bin/sh" == Right (Run (ECmd "/bin/sh" [])))
  , ("external: a bound head is a function", parseCommand names "f y" == Right (Run (EApp (EVar "f") (EVar "y"))))
  , ("external: a bound select stays a select", parseCommand names "x.a" == Right (Run (ESelect (EVar "x") "a")))
  , ("external: a lambda parameter is bound", parseCommand names "g = h -> h 1" == Right (Bind "g" (ELam "h" (EApp (EVar "h") (ELit (LInt 1))))))
  , ("external: a let name is bound in its body", parseCommand names "let h = 1 in h" == Right (Run (ELet "h" (ELit (LInt 1)) (EVar "h"))))
  , ("external: not bound past its scope", parseCommand names "(h -> h) (h)" == Right (Run (EApp (ELam "h" (EVar "h")) (ECmd "h" []))))
  , ("external: inside a lambda", parseCommand names "g = a -> echo $a" == Right (Bind "g" (ELam "a" (ECmd "echo" [AVar "a"]))))
  , ("external: ends at a bracket", parseCommand names "(echo a) " == Right (Run (ECmd "echo" [AWord "a"])))
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
  , ("do: a value is not a statement", "type mismatch" `isInfixOf` final ["do 1 end"])
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
  , ("with: needs an overlay", "type mismatch" `isInfixOf` final ["with 1 do print 1 end"])
  , ("with: use inside it changes the session", promptAfter ["with (overlay \"o\" []) do use (overlay \"u\" []) end"] == "λ [u]> ")
  , ("needsMore: an open block", needsMore "do print 1;" && needsMore "with o do" && not (needsMore "do print 1 end"))
  , ("needsMore: strings and words", not (needsMore "echo \"do\"") && not (needsMore "echo done") && needsMore "x <- do")
  , ("ref: a reference is a String", ty "unuse nixpkgs#jq" "Comp {env} ()")
  , ("ref: use resolves with pkg", ty "use nixpkgs#jq" "Comp {exec nix, env} ()")
  , ("ref: with resolves with pkg", ty "with nixpkgs#ffmpeg do ffmpeg end" "Comp {exec nix} Status")
  , ("ref: dev takes one", ty "dev .#bench" "Comp {exec bash, exec nix} Overlay")
  , ("ref: an external argument is still a word", parseCommand names "nix build nixpkgs#jq" == Right (Run (ECmd "nix" [AWord "build", AWord "nixpkgs#jq"])))
  , ("ref: unuse by reference", final ["use (overlay \"nixpkgs#jq\" [])", "unuse nixpkgs#jq", "environments"] == "[]")
  , ("paths: ls takes a bareword", parseCommand sigs "ls src" == Right (Run (EApp (EVar "ls") (ELit (LStr "src")))))
  , ("paths: ls alone lists here", parseCommand sigs "ls" == Right (Run (EApp (EVar "ls") (ELit (LStr ".")))))
  , ("paths: ls before a pipe lists here", parseCommand sigs "ls | collect" == Right (Run (EPipe (EApp (EVar "ls") (ELit (LStr "."))) (EVar "collect"))))
  , ("paths: cd alone goes home", parseCommand sigs "cd" == Right (Run (EApp (EVar "cd") (ELit (LStr "~")))))
  , ("paths: .. and / are paths", parseCommand sigs "cd .." == Right (Run (EApp (EVar "cd") (ELit (LStr "..")))) && parseCommand sigs "cd /tmp" == Right (Run (EApp (EVar "cd") (ELit (LStr "/tmp")))))
  , ("paths: $x is a variable", parseCommand sigs "cd $d" == Right (Run (EApp (EVar "cd") (EVar "d"))))
  , ("paths: a string", parseCommand sigs "cd \"a b\"" == Right (Run (EApp (EVar "cd") (ELit (LStr "a b")))))
  , ("paths: parentheses hold an expression", parseCommand sigs "cd (f x)" == Right (Run (EApp (EVar "cd") (EApp (EVar "f") (EVar "x")))))
  , ("paths: a rebound builtin has no signature", parseCommand (BoundWith (Set.fromList ["ls", "f"]) Set.empty) "ls f" == Right (Run (EApp (EVar "ls") (EVar "f"))))
  , ("paths: a lambda parameter hides a signature", parseCommand sigs "ls -> ls f" == Right (Run (ELam "ls" (EApp (EVar "ls") (EVar "f")))))
  , ("paths: other builtins take expressions", parseCommand sigs "f .size" == Right (Run (EApp (EVar "f") (ELam "r" (ESelect (EVar "r") "size")))))
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
  , ("bg: a computation too", ty "pure 3 &" "Comp (Job Int)")
  , ("bg: wait gives the result", ty "x -> wait x" "Job { | a} b -> Comp { | a} b")
  , ("bg: fg is the same shape", ty "x -> fg x" "Job { | a} b -> Comp { | a} b")
  , ("bg: a value is not a job", "type mismatch" `isInfixOf` final ["wait 1"])
  , ("bg: & ends a word", parseCommand names "echo a&" == Right (Run (EBg (ECmd "echo" [AWord "a"]))))
  , ("bg: wait", final ["j <- pure 42 &", "wait j"] == "42")
  , ("bg: wait twice", drop 1 (session ["j <- sh -c \"exit 4\" &", "wait j", "wait j"]) == ["exit 4", "exit 4"])
  , ("bg: a block as a job", final ["j <- do x <- pure 1; pure (add x 1) end &", "wait j"] == "2")
  , ("bg: a failed job fails wait", final ["j <- nope-xyz &"] == "command not found: nope-xyz")
  , ("bg: the handle shows its number", final ["j <- pure 1 &", "j"] == "<job 1>")
  , ("bg: fg as a command still works", parseCommand (BoundWith (Set.fromList ["fg"]) (Set.fromList ["fg"])) "fg 2" == Right (Foreground (Just 2)))
  , ("bg: fg of an expression is the function", parseCommand (BoundWith (Set.fromList ["fg", "j"]) (Set.fromList ["fg"])) "fg j" == Right (Run (EApp (EVar "fg") (EVar "j"))))
  , (":env: nothing active", final [":env"] == "no active environments")
  , (":env: lists patches", final [devLine, "use dev", ":env"] == "dev\n  set \"LSH_A\" \"1\"\n  set \"LSH_B\" \"x\"")
  , (":env: marks shadowing", ("⚠ shadows dev" `isInfixOf`) (final [devLine, "use dev", "use (overlay \"b\" [set \"LSH_A\" \"2\"])", ":env"]))
  , (":env: a variable and its layers", final [devLine, "use dev", "use (overlay \"b\" [set \"LSH_A\" \"2\"])", ":env LSH_A"] == "LSH_A = \"2\"\n  base  unset\n  dev  set \"LSH_A\" \"1\"\n  b  set \"LSH_A\" \"2\"")
  , (":env: an unset variable", final [":env LSH_NOPE"] == "LSH_NOPE is unset\n  base  unset")
  , (":env: a prepend shows the base", ("/lsh-x:" `isInfixOf`) (final ["use (overlay \"p\" [prepend \"PATH\" \"/lsh-x\"])", ":env PATH"]))
  ]
 where
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
  -- Tests must not read or write the user's approvals: an approved flake in
  -- the directory the tests start in would otherwise activate, hook and all.
  temporary <- getTemporaryDirectory
  let scratch = temporary <> "/lambdash-repl-tests"
  exists <- doesDirectoryExist scratch
  when exists (removeDirectoryRecursive scratch)
  createDirectoryIfMissing True scratch
  setEnv "XDG_STATE_HOME" scratch
  results <- mapM runTest tests
  putStrLn (show (length (filter id results)) <> "/" <> show (length results) <> " passed")
  unless (and results) exitFailure
