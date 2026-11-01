-- | REPL tests. Each runs lines through 'step', as the loop does, and compares
-- the text it would show.
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (unless)
import Data.List (isInfixOf)
import Lambdash.Infer
import Lambdash.Repl
import Lambdash.Unify
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Directory (removeFile)
import System.Exit (exitFailure)
import System.IO (hClose, hFlush, openTempFile, stdout)
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

ty :: String -> String -> Bool
ty input expected = session [":t " <> input] == [expected]

v0, v1 :: Var
v0 = Var 7
v1 = Var 3

tests :: [(String, Bool)]
tests =
  [ ("command: blank", parseCommand "" == Right Blank && parseCommand "  -- note" == Right Blank)
  , ("command: quit", parseCommand ":q" == Right Exit)
  , ("command: an expression", parseCommand "x" == Right (Run (EVar "x")))
  , ("command: an application is not a binding", parseCommand "f x" == Right (Run (EApp (EVar "f") (EVar "x"))))
  , ("command: a binding", parseCommand "x = 1" == Right (Bind "x" (ELit (LInt 1))))
  , ("command: a lambda binding", parseCommand "i = x -> x" == Right (Bind "i" (ELam "x" (EVar "x"))))
  , ("command: a type query", parseCommand ":t x" == Right (TypeOf (EVar "x")))
  , ("command: let is still an expression", parseCommand "let x = 1 in x" == Right (Run (ELet "x" (ELit (LInt 1)) (EVar "x"))))
  , ("command: an unknown command", either (const True) (const False) (parseCommand ":nope"))
  , ("command: a binding needs a value", either (const True) (const False) (parseCommand "x ="))
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
  , ("step: let does not leak", take 1 (reverse (session ["let x = 1 in x", "x"])) == ["unbound name: x"])
  , ("step: a type query", ty "1" "Int")
  , ("step: a type query for a polymorphic function", ty "x -> x" "a -> a")
  , ("step: a type query for an accessor", ty ".size" "{size : a | b} -> a")
  , ("step: a type query sees bindings", final ["x = 1", ":t x"] == "Int")
  , ("step: a type query runs nothing and binds nothing", final [":t y -> y", "y"] == "unbound name: y")
  , ("step: a checker error", session ["not 1"] == ["type mismatch: Bool and Int"])
  , ("step: a parse error shows something", not (null (final ["(1"])))
  , ("step: a failed binding binds nothing", final ["x = not 1", "x"] == "unbound name: x")
  , ("step: a failed binding keeps the old value", final ["x = 1", "x = not 1", "x"] == "1")
  , ("step: errors do not end a session", final ["nope", "1"] == "1")
  , ("command: a run binding", parseCommand "x <- y" == Right (BindRun "x" (EVar "y")))
  , ("renderType: a computation", renderType (TComp TInt) == "Comp Int")
  , ("renderType: unit", renderType (TComp TUnit) == "Comp ()")
  , ("renderType: a computation of a function", renderType (TComp (TFun TInt TInt)) == "Comp (Int -> Int)")
  , ("renderType: a computation of a computation", renderType (TComp (TComp TInt)) == "Comp (Comp Int)")
  , ("renderType: a computation of a record", renderType (TComp (TRecord REmpty)) == "Comp {}")
  , ("renderType: a function that gives a computation", renderType (TFun TInt (TComp TInt)) == "Int -> Comp Int")
  , ("comp: print shows through stdout", prints ["print 1"] == ("1\n", [""]))
  , ("comp: pure shows its result", prints ["pure 1"] == ("", ["1"]))
  , ("comp: a unit result shows nothing", prints ["pure ()"] == ("", [""]))
  , ("comp: unit as a value shows", prints ["()"] == ("", ["()"]))
  , ("comp: = runs nothing", prints ["gs = print 1"] == ("", ["gs : Comp ()"]))
  , ("comp: each bare line runs it", prints ["gs = print 1", "gs", "gs"] == ("1\n1\n", ["gs : Comp ()", "", ""]))
  , ("comp: a type query runs nothing", prints [":t print 1"] == ("", ["Comp ()"]))
  , ("comp: a function is not a computation", prints ["f = x -> print x"] == ("", ["f : a -> Comp ()"]))
  , ("comp: applying a function runs nothing", prints ["f = x -> print x", "g = f 1"] == ("", ["f : a -> Comp ()", "g : Comp ()"]))
  , ("comp: then a bare line runs it", fst (prints ["f = x -> print x", "g = f 1", "g"]) == "1\n")
  , ("comp: <- runs it", prints ["x <- print 1"] == ("1\n", ["x : ()"]))
  , ("comp: <- binds the result", final ["x <- pure 1", "add x 1"] == "2")
  , ("comp: <- runs once", fst (prints ["x <- print 1", "x", "x"]) == "1\n")
  , ("comp: <- refuses a value", final ["x <- 1"] == "type mismatch: Int and Comp a")
  , ("comp: a refused <- binds nothing", final ["x <- 1", "x"] == "unbound name: x")
  , ("comp: a failed <- runs nothing", prints ["x <- print (not 1)"] == ("", ["type mismatch: Bool and Int"]))
  , ("comp: only the outer computation runs", prints ["pure (print 1)"] == ("", ["<computation>"]))
  , ("comp: an inner computation can run later", fst (prints ["c <- pure (print 1)", "c"]) == "1\n")
  , ("comp: a run result stays polymorphic", final ["i <- pure (x -> x)", "(a b -> a) (i 1) (i true)"] == "1")
  , ("comp: a computation in a record runs by selection", fst (prints ["r = {a = print 1}", "r", "r.a"]) == "1\n")
  , ("comp: a computation prints on each run", fst (prints ["gs = print (add 1 2)", "gs", "gs"]) == "3\n3\n")
  ]

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
