-- | REPL tests. Each runs lines through 'step', as the loop does, and compares
-- the text it would show.
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (unless)
import Data.List (isInfixOf)
import Data.Set qualified as Set
import Lambdash.Infer
import Lambdash.Parser (Bound (..))
import Lambdash.Repl
import Lambdash.Unify
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Directory (removeFile)
import System.Exit (exitFailure)
import System.IO (hClose, hFlush, openTempFile, stdout)
import System.Environment (lookupEnv)
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
  go s [] = pure (sessionPrompt s)
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
  , ("command: jobs", parseCommand names "jobs" == Right ListJobs)
  , ("command: fg", parseCommand names "fg" == Right (Foreground Nothing))
  , ("command: fg with a number", parseCommand names "fg 2" == Right (Foreground (Just 2)) && parseCommand names "fg %2" == Right (Foreground (Just 2)))
  , ("command: a bound fg is a value", parseCommand (BoundNames (Set.fromList ["fg"])) "fg" == Right (Run (EVar "fg")))
  , ("command: fg with a word is a command line", parseCommand names "fg x" == Right (Run (ECmd "fg" [AWord "x"])))
  , ("step: fg with no jobs", session ["fg"] == ["no stopped jobs"] && session ["fg 3"] == ["no job 3"])
  , ("step: jobs with no jobs", session ["jobs"] == [""])
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
  , ("env: shadowing warns once", session [devLine, "use dev", "use (overlay \"b\" [set \"LSH_A\" \"2\"])", "print 1"] == ["dev : Overlay", "", "warning: b shadows LSH_A from dev", ""])
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
  , (":env: nothing active", final [":env"] == "no active environments")
  , (":env: lists patches", final [devLine, "use dev", ":env"] == "dev\n  set \"LSH_A\" \"1\"\n  set \"LSH_B\" \"x\"")
  , (":env: marks shadowing", ("⚠ shadows dev" `isInfixOf`) (final [devLine, "use dev", "use (overlay \"b\" [set \"LSH_A\" \"2\"])", ":env"]))
  , (":env: a variable and its layers", final [devLine, "use dev", "use (overlay \"b\" [set \"LSH_A\" \"2\"])", ":env LSH_A"] == "LSH_A = \"2\"\n  base  unset\n  dev  set \"LSH_A\" \"1\"\n  b  set \"LSH_A\" \"2\"")
  , (":env: an unset variable", final [":env LSH_NOPE"] == "LSH_NOPE is unset\n  base  unset")
  , (":env: a prepend shows the base", ("/lsh-x:" `isInfixOf`) (final ["use (overlay \"p\" [prepend \"PATH\" \"/lsh-x\"])", ":env PATH"]))
  ]
 where
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
  results <- mapM runTest tests
  putStrLn (show (length (filter id results)) <> "/" <> show (length results) <> " passed")
  unless (and results) exitFailure
