-- | Evaluator tests. Each runs the whole road: parse, check, evaluate, render.
-- An input that does not parse or check is a failure, so the checker guards
-- every test here.
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (unless)
import Lambdash.Eval
import Lambdash.Infer
import Lambdash.Parser
import Lambdash.Unify
import System.Exit (exitFailure)
import System.Timeout (timeout)

gives :: String -> String -> Bool
gives input expected = case parseExpr input of
  Left _ -> False
  Right e -> case inferIn typeEnv e of
    Left _ -> False
    Right _ -> render (eval scope e) == expected

tests :: [(String, Bool)]
tests =
  [ ("render: int", render (VInt 1) == "1")
  , ("render: negative int", render (VInt (-1)) == "-1")
  , ("render: bool", render (VBool True) == "true" && render (VBool False) == "false")
  , ("render: empty record", render (VRecord []) == "{}")
  , ("render: record in written order", render (VRecord [("b", VInt 1), ("a", VBool True)]) == "{b = 1, a = true}")
  , ("render: nested record", render (VRecord [("a", VRecord [("b", VInt 1)])]) == "{a = {b = 1}}")
  , ("render: builtin", render (VBuiltin "not" id) == "<function>")
  , ("select: finds a field", render (select "b" (VRecord [("a", VInt 1), ("b", VInt 2)])) == "2")
  , ("select: the first of two", render (select "a" (VRecord [("a", VInt 1), ("a", VInt 2)])) == "1")
  , ("applyValue: builtin", render (applyValue (VBuiltin "f" id) (VInt 1)) == "1")
  , ("eval: literals", "1" `gives` "1" && "true" `gives` "true")
  , ("eval: a variable from the builtins", "not" `gives` "<function>")
  , ("eval: builtin application", "not true" `gives` "false")
  , ("eval: partial application", "add 1" `gives` "<function>")
  , ("eval: full application", "add 1 2" `gives` "3")
  , ("eval: identity", "(x -> x) 1" `gives` "1")
  , ("eval: a lambda is a value", "x -> x" `gives` "<function>")
  , ("eval: two parameters", "(x y -> x) 1 2" `gives` "1")
  , ("eval: let", "let x = 1 in x" `gives` "1")
  , ("eval: let shadows", "let x = 1 in let x = true in x" `gives` "true")
  , ("eval: a parameter shadows", "let x = 1 in (x -> x) true" `gives` "true")
  , ("eval: an inner let ends with its scope", "let x = 1 in (let x = true in not x) " `gives` "false")
  , ("eval: let polymorphism", "let i = x -> x in (a b -> a) (i 1) (i true)" `gives` "1")
  , ("eval: record", "{a = add 1 2, b = not true}" `gives` "{a = 3, b = false}")
  , ("eval: select", "{a = 1, b = true}.b" `gives` "true")
  , ("eval: nested select", "{a = {b = 2}}.a.b" `gives` "2")
  , ("eval: accessor", "(.a) {a = 1}" `gives` "1")
  , ("eval: accessor as an argument", "(f -> f {size = 1}) .size" `gives` "1")
  , ("eval: a record holds a function", "{f = x -> x}.f 1" `gives` "1")
  , ("closure: the warm-up", "let x = 1 in let f = y -> x in let x = true in f 0" `gives` "1")
  , ("closure: captures a parameter", "(x -> y -> x) 1 2" `gives` "1")
  , ("closure: each one keeps its own", "let k = x -> y -> x in let a = k 1 in let b = k 2 in a 0" `gives` "1")
  , ("closure: the parameter wins over the capture", "let x = 1 in (f -> f 2) (x -> x)" `gives` "2")
  , ("closure: an argument uses the caller's scope", "let y = 1 in let f = x -> y in let y = 2 in (z -> f z) y" `gives` "1")
  , ("closure: an argument comes from where it is written", "let y = 1 in (f -> let y = 2 in f y) (x -> x)" `gives` "2")
  , ("closure: survives a record", "let r = (x -> {f = y -> x}) 1 in let x = 2 in r.f 0" `gives` "1")
  , ("closure: compose", "let c = f g x -> f (g x) in c not not true" `gives` "true")
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
