-- | Parser tests. Each piece runs on its own, so TODO shows what is left.
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (unless)
import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Lambdash.Infer
import Lambdash.Parser
import Lambdash.Unify
import System.Exit (exitFailure)
import System.Timeout (timeout)
import Text.Megaparsec (eof)

run :: Parser a -> String -> Maybe a
run p s = either (const Nothing) Just (runIn AllBound (p <* eof) s)

v :: Name -> Expr
v = EVar

int :: Int -> Expr
int = ELit . LInt

is :: String -> Expr -> Bool
is s e = parseExpr s == Right e

bad :: String -> Bool
bad = isLeft . parseExpr

-- | The whole road: text in, type out.
typed :: String -> Ty -> Bool
typed s t = case parseExpr s of
  Right e -> inferIn prelude e == Right t
  Left _ -> False

prelude :: Env
prelude = Map.fromList [("not", mono (TFun TBool TBool))]

tests :: [(String, Bool)]
tests =
  [ ("keyword: matches", run (keyword "let") "let" == Just ())
  , ("keyword: not a prefix of a name", run (keyword "let") "letter" == Nothing)
  , ("identifier: letters", run identifier "abc" == Just "abc")
  , ("identifier: digits, _ and '", run identifier "a_1'" == Just "a_1'")
  , ("identifier: not from a digit", run identifier "1a" == Nothing)
  , ("identifier: refuses reserved words", all (\w -> run identifier w == Nothing) ["let", "in", "true", "false"])
  , ("identifier: reserved word as a prefix", run identifier "letter" == Just "letter" && run identifier "trueish" == Just "trueish")
  , ("identifier: leaves trailing space", run identifier "a " == Nothing)
  , ("literal: integer", run literal "42" == Just (LInt 42))
  , ("literal: true", run literal "true" == Just (LBool True))
  , ("literal: false", run literal "false" == Just (LBool False))
  , ("literal: not a name", run literal "trueish" == Nothing)
  , ("literal: a string", run literal "\"a b\"" == Just (LStr "a b"))
  , ("literal: an escape", run literal "\"a\\n\"" == Just (LStr "a\n"))
  , ("expr: AllBound has no external commands", is "ls x" (EApp (v "ls") (v "x")))
  , ("record: empty", run record "{}" == Just (ERecord []))
  , ("record: fields in order", run record "{ a = 1, b = true }" == Just (ERecord [("a", int 1), ("b", ELit (LBool True))]))
  , ("record: fields hold any expression", run record "{f = x -> x}" == Just (ERecord [("f", ELam "x" (v "x"))]))
  , ("record: leaves trailing space", run record "{} " == Nothing)
  , ("accessor: one label", run accessor ".size" == Just (ELam "r" (ESelect (v "r") "size")))
  , ("accessor: a path", run accessor ".a.b" == Just (ELam "r" (ESelect (ESelect (v "r") "a") "b")))
  , ("atom: variable", run atom "x" == Just (v "x"))
  , ("atom: literal", run atom "1" == Just (int 1))
  , ("atom: parentheses", run atom "( x )" == Just (v "x"))
  , ("atom: unit", run atom "()" == Just (ELit LUnit) && run atom "( )" == Just (ELit LUnit))
  , ("atom: record", run atom "{}" == Just (ERecord []))
  , ("atom: accessor", run atom ".a" == Just (ELam "r" (ESelect (v "r") "a")))
  , ("atom: leaves trailing space", run atom "x " == Nothing)
  , ("postfix: select", run postfix "r.a" == Just (ESelect (v "r") "a"))
  , ("postfix: a chain goes to the left", run postfix "r.a.b" == Just (ESelect (ESelect (v "r") "a") "b"))
  , ("postfix: on parentheses", run postfix "(r).a" == Just (ESelect (v "r") "a"))
  , ("postfix: on a record", run postfix "{a = 1}.a" == Just (ESelect (ERecord [("a", int 1)]) "a"))
  , ("postfix: skips trailing space", run postfix "x " == Just (v "x"))
  , ("application: one", run application "f" == Just (v "f"))
  , ("application: to the left", run application "f x y" == Just (EApp (EApp (v "f") (v "x")) (v "y")))
  , ("application: a space makes an accessor argument", run application "f .a" == Just (EApp (v "f") (ELam "r" (ESelect (v "r") "a"))))
  , ("application: no space makes a select", run application "f.a" == Just (ESelect (v "f") "a"))
  , ("application: select as an argument", run application "f r.a" == Just (EApp (v "f") (ESelect (v "r") "a")))
  , ("lambda: one parameter", run lambda "x -> x" == Just (ELam "x" (v "x")))
  , ("lambda: two parameters", run lambda "x y -> x" == Just (ELam "x" (ELam "y" (v "x"))))
  , ("lambda: the body goes right", run lambda "x -> f x y" == Just (ELam "x" (EApp (EApp (v "f") (v "x")) (v "y"))))
  , ("lambda: nests", run lambda "x -> y -> x" == Just (ELam "x" (ELam "y" (v "x"))))
  , ("letIn: simple", run letIn "let x = 1 in x" == Just (ELet "x" (int 1) (v "x")))
  , ("letIn: nests in the body", run letIn "let x = 1 in let y = 2 in x" == Just (ELet "x" (int 1) (ELet "y" (int 2) (v "x"))))
  , ("letIn: a lambda binding", run letIn "let i = x -> x in i i" == Just (ELet "i" (ELam "x" (v "x")) (EApp (v "i") (v "i"))))
  , ("expr: a name can start with a keyword", is "letter" (v "letter") && is "f inner" (EApp (v "f") (v "inner")))
  , ("expr: application is not a lambda", is "f x" (EApp (v "f") (v "x")))
  , ("expr: lambda in parentheses as an argument", is "f (x -> x) 1" (EApp (EApp (v "f") (ELam "x" (v "x"))) (int 1)))
  , ("expr: leading space and comments", is "  -- hello\n  x -- bye" (v "x"))
  , ("expr: in stops an application", is "let a = f x in a" (ELet "a" (EApp (v "f") (v "x")) (v "a")))
  , ("error: empty line", bad "")
  , ("error: trailing input", bad "x )")
  , ("error: unclosed parenthesis", bad "(x")
  , ("error: let with no in", bad "let x = 1")
  , ("error: arrow with no parameter", bad "-> x")
  , ("error: keyword as a parameter", bad "in -> 1")
  , ("error: record with no value", bad "{a}")
  , ("typed: identity on an int", typed "(x -> x) 1" TInt)
  , ("typed: polymorphic let", typed "let i = x -> x in (a b -> a) (i 1) (i true)" TInt)
  , ("typed: select from a record", typed "{name = 1, ok = true}.ok" TBool)
  , ("typed: an accessor as an argument", typed "(f -> f {size = 1}) .size" TInt)
  , ("typed: not", typed "not true" TBool)
  , ("do: statements", is "do x <- f; y = x; f y end" (EDo [SBind "x" (v "f"), SLet "y" (v "x"), SRun (EApp (v "f") (v "y"))]))
  , ("do: one statement", is "do x end" (EDo [SRun (v "x")]))
  , ("do: a trailing ;", is "do x; end" (EDo [SRun (v "x")]))
  , ("do: an argument", is "f do x end" (EApp (v "f") (EDo [SRun (v "x")])))
  , ("do: nested", is "do do x end end" (EDo [SRun (EDo [SRun (v "x")])]))
  , ("with: an overlay and a block", is "with o do x end" (EWith (v "o") (EDo [SRun (v "x")])))
  , ("error: do with no end", bad "do x")
  , ("error: an empty block", bad "do end")
  , ("error: a block that ends with a binding", bad "do x <- f end")
  , ("error: with needs a block", bad "with o x")
  , ("error: end is reserved", bad "end")
  , ("bg: a trailing & backgrounds the expression", is "f x &" (EBg (EApp (v "f") (v "x"))))
  , ("bg: it takes the whole pipeline", is "a | b &" (EBg (EPipe (v "a") (v "b"))))
  , ("bg: && is not &", bad "f &&")
  , ("ref: a flake reference is a String literal", is "f nixpkgs#ffmpeg" (EApp (v "f") (ELit (LRef "nixpkgs#ffmpeg"))))
  , ("ref: the current flake", is "f .#" (EApp (v "f") (ELit (LRef ".#"))) && is "f .#bench" (EApp (v "f") (ELit (LRef ".#bench"))))
  , ("ref: paths and URLs", is "f ./a/b#dev" (EApp (v "f") (ELit (LRef "./a/b#dev"))) && is "f github:o/r?ref=x#y" (EApp (v "f") (ELit (LRef "github:o/r?ref=x#y"))))
  , ("ref: an accessor is still an accessor", is "f .size" (EApp (v "f") (ELam "r" (ESelect (v "r") "size"))))
  , ("ref: use means pkg", is "use nixpkgs#jq" (EDo [SBind " overlay" (EApp (v "pkg") (ELit (LStr "nixpkgs#jq"))), SRun (EApp (v "use") (v " overlay"))]))
  , ("ref: use of a string is not sugar", is "use \"x#y\"" (EApp (v "use") (ELit (LStr "x#y"))))
  , ("ref: with resolves first", is "with .#b do x end"
      (EDo [SBind " overlay" (EApp (v "pkg") (ELit (LStr ".#b"))), SRun (EWith (v " overlay") (EDo [SRun (v "x")]))]))
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
