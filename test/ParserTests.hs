-- | Parser tests. Each piece runs on its own, so TODO shows what is left.
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (unless)
import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Lambdash.Infer
import Lambdash.Desugar (desugar, stripLocations)
import Lambdash.Parser
import Lambdash.Syntax (Declaration (..), Pattern (..), SPred (..), SRow (..), SType (..), Surface)
import Lambdash.Unify
import System.Exit (exitFailure)
import System.Timeout (timeout)
import Text.Megaparsec (eof)

run :: Parser a -> String -> Maybe a
run p s = either (const Nothing) Just (runIn AllBound (p <* eof) s)

-- | A surface parser, desugared to core without spans, so that tests compare
-- shapes.
core :: Parser Surface -> String -> Maybe Expr
core p s = stripLocations . desugar <$> run p s

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
  , ("record: empty", core record "{}" == Just (ERecord []))
  , ("record: fields in order", core record "{ a = 1, b = true }" == Just (ERecord [("a", int 1), ("b", ELit (LBool True))]))
  , ("record: fields hold any expression", core record "{f = x -> x}" == Just (ERecord [("f", ELam "x" (v "x"))]))
  , ("record: leaves trailing space", core record "{} " == Nothing)
  , ("accessor: one label", core accessor ".size" == Just (ELam "r" (ESelect (v "r") "size")))
  , ("accessor: a path", core accessor ".a.b" == Just (ELam "r" (ESelect (ESelect (v "r") "a") "b")))
  , ("atom: variable", core atom "x" == Just (v "x"))
  , ("atom: literal", core atom "1" == Just (int 1))
  , ("atom: parentheses", core atom "( x )" == Just (v "x"))
  , ("atom: unit", core atom "()" == Just (ELit LUnit) && core atom "( )" == Just (ELit LUnit))
  , ("atom: record", core atom "{}" == Just (ERecord []))
  , ("atom: accessor", core atom ".a" == Just (ELam "r" (ESelect (v "r") "a")))
  , ("atom: leaves trailing space", core atom "x " == Nothing)
  , ("postfix: select", core postfix "r.a" == Just (ESelect (v "r") "a"))
  , ("postfix: a chain goes to the left", core postfix "r.a.b" == Just (ESelect (ESelect (v "r") "a") "b"))
  , ("postfix: on parentheses", core postfix "(r).a" == Just (ESelect (v "r") "a"))
  , ("postfix: on a record", core postfix "{a = 1}.a" == Just (ESelect (ERecord [("a", int 1)]) "a"))
  , ("postfix: skips trailing space", core postfix "x " == Just (v "x"))
  , ("application: one", core application "f" == Just (v "f"))
  , ("application: to the left", core application "f x y" == Just (EApp (EApp (v "f") (v "x")) (v "y")))
  , ("application: a space makes an accessor argument", core application "f .a" == Just (EApp (v "f") (ELam "r" (ESelect (v "r") "a"))))
  , ("application: no space makes a select", core application "f.a" == Just (ESelect (v "f") "a"))
  , ("application: select as an argument", core application "f r.a" == Just (EApp (v "f") (ESelect (v "r") "a")))
  , ("lambda: one parameter", core lambda "x -> x" == Just (ELam "x" (v "x")))
  , ("lambda: two parameters", core lambda "x y -> x" == Just (ELam "x" (ELam "y" (v "x"))))
  , ("lambda: the body goes right", core lambda "x -> f x y" == Just (ELam "x" (EApp (EApp (v "f") (v "x")) (v "y"))))
  , ("lambda: nests", core lambda "x -> y -> x" == Just (ELam "x" (ELam "y" (v "x"))))
  , ("letIn: simple", core letIn "let x = 1 in x" == Just (ELet "x" (int 1) (v "x")))
  , ("letIn: nests in the body", core letIn "let x = 1 in let y = 2 in x" == Just (ELet "x" (int 1) (ELet "y" (int 2) (v "x"))))
  , ("letIn: a lambda binding", core letIn "let i = x -> x in i i" == Just (ELet "i" (ELam "x" (v "x")) (EApp (v "i") (v "i"))))
  , ("let rec: one function", core letIn "let rec f = x -> f x in f" == Just (ELetRec [("f", ELam "x" (EApp (v "f") (v "x")))] (v "f")))
  , ("let rec: and joins a group", core letIn "let rec f = x -> g x and g = y -> f y in g" == Just (ELetRec [("f", ELam "x" (EApp (v "g") (v "x"))), ("g", ELam "y" (EApp (v "f") (v "y")))] (v "g")))
  , ("let rec: an annotated lambda", not (bad "let rec f = (x -> f x : Int -> Int) in f"))
  , ("let rec: a later name is a call, not a command", parseExprIn (BoundNames Set.empty) "let rec f = x -> g x and g = y -> y in f" == Right (ELetRec [("f", ELam "x" (EApp (v "g") (v "x"))), ("g", ELam "y" (v "y"))] (v "f")))
  , ("let rec: only lambdas", bad "let rec x = 1 in x" && bad "let rec f = x -> x and y = f in y")
  , ("let rec: needs in in an expression", bad "let rec f = x -> x")
  , ("identifier: rec and and are reserved", run identifier "rec" == Nothing && run identifier "and" == Nothing && run identifier "record" == Just "record")
  , ("case: arms", is "case x of Just y -> y; _ -> 0 end" (ECase (v "x") [(PCon "Just" [PVar "y"], v "y"), (PWild, int 0)]))
  , ("case: a ; before end", is "case x of y -> y; end" (ECase (v "x") [(PVar "y", v "y")]))
  , ("case: an arm binds its names", parseExprIn (BoundNames Set.empty) "case 1 of x -> x end" == Right (ECase (int 1) [(PVar "x", v "x")]))
  , ("case: of ends a command", parseExprIn (BoundNames Set.empty) "case git status of _ -> 0 end" == Right (ECase (ECmd "git" [AWord "status"]) [(PWild, int 0)]))
  , ("case: needs an arm and end", bad "case x of end" && bad "case x of y -> y")
  , ("pattern: nested constructors", run casePattern "Cons x (Cons y _)" == Just (PCon "Cons" [PVar "x", PCon "Cons" [PVar "y", PWild]]))
  , ("pattern: literals", map (run casePattern) ["-1", "\"s\"", "true", "()"] == map Just [PLit (LInt (-1)), PLit (LStr "s"), PLit (LBool True), PLit LUnit])
  , ("pattern: a constructor alone", run casePattern "Nothing" == Just (PCon "Nothing" []))
  , ("pattern: records, with puns", run casePattern "{name = Just n, size}" == Just (PRecord [("name", PCon "Just" [PVar "n"]), ("size", PVar "size")]) && run casePattern "{}" == Just (PRecord []))
  , ("identifier: case and of are reserved", run identifier "case" == Nothing && run identifier "of" == Nothing && run identifier "offset" == Just "offset")
  , ("data: a declaration", run declaration "data Maybe a = Nothing | Just a" == Just (DeclData "Maybe" ["a"] [("Nothing", []), ("Just", [STVar "a"])]))
  , ("data: recursive fields", run declaration "data List a = Nil | Cons a (List a)" == Just (DeclData "List" ["a"] [("Nil", []), ("Cons", [STVar "a", STApp "List" Nothing [STVar "a"]])]))
  , ("type: a declared type takes arguments", run typeExpr "Maybe Int -> Either e a" == Just (STFun (STApp "Maybe" Nothing [STCon "Int"]) (STApp "Either" Nothing [STVar "e", STVar "a"])))
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
  , ("typed: identity", typed "(x -> x) true" TBool)
  , ("typed: polymorphic let", typed "let i = x -> x in (a b -> a) (i \"s\") (i true)" TString)
  , ("typed: select from a record", typed "{name = 1, ok = true}.ok" TBool)
  , ("typed: an accessor as an argument", typed "(f -> f {size = \"s\"}) .size" TString)
  , ("typed: not", typed "not true" TBool)
  , ("do: statements", is "do x <- f; y = x; f y end" (EBind "x" (v "f") (ELet "y" (v "x") (ERun (EApp (v "f") (v "y"))))))
  , ("do: one statement", is "do x end" (ERun (v "x")))
  , ("do: a trailing ;", is "do x; end" (ERun (v "x")))
  , ("do: an argument", is "f do x end" (EApp (v "f") (ERun (v "x"))))
  , ("do: nested", is "do do x end end" (ERun (ERun (v "x"))))
  , ("do: a dropped result gets a name nothing can write", is "do a; b end" (EBind "%1" (v "a") (ERun (v "b"))))
  , ("do: invented names are distinct", is "do a; b; c end" (EBind "%1" (v "a") (EBind "%2" (v "b") (ERun (v "c")))))
  , ("with: an overlay and a block", is "with o do x end" (EWith (v "o") (ERun (v "x"))))
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
  , ("ref: use means pkg, through the builtins themselves", is "use nixpkgs#jq" (EBind "%1" (EApp (v "%pkg") (ELit (LStr "nixpkgs#jq"))) (ERun (EApp (v "%use") (v "%1")))))
  , ("ref: useLocal too", is "useLocal .#" (EBind "%1" (EApp (v "%pkg") (ELit (LStr ".#"))) (ERun (EApp (v "%useLocal") (v "%1")))))
  , ("ref: a rebound use is an ordinary function", parseExprIn (BoundWith (Set.fromList ["use"]) Set.empty) "use nixpkgs#jq" == Right (EApp (v "use") (ELit (LRef "nixpkgs#jq"))))
  , ("ref: use of a string is not sugar", is "use \"x#y\"" (EApp (v "use") (ELit (LStr "x#y"))))
  , ("ref: with resolves first", is "with .#b do x end"
      (EBind "%1" (EApp (v "%pkg") (ELit (LStr ".#b"))) (ERun (EWith (v "%1") (ERun (v "x"))))))
  , ("op: precedence", is "1 + 2 * 3" (op "+" (int 1) (op "*" (int 2) (int 3))))
  , ("op: left associative", is "a - b - c" (op "-" (op "-" (v "a") (v "b")) (v "c")))
  , ("op: right associative", is "a <> b <> c" (op "<>" (v "a") (op "<>" (v "b") (v "c"))))
  , ("op: comparisons do not chain", bad "a == b == c" && bad "a < b > c")
  , ("op: looser than application", is "f x + g y" (op "+" (EApp (v "f") (v "x")) (EApp (v "g") (v "y"))))
  , ("op: tighter than a pipe", is "a + 1 | f" (EPipe (op "+" (v "a") (int 1)) (v "f")))
  , ("op: || and && are operators", is "a || b && c" (op "||" (v "a") (op "&&" (v "b") (v "c"))))
  , ("op: && is not a job", is "a && b" (op "&&" (v "a") (v "b")))
  , ("op: a lambda is the last operand", is "f >>> x -> x" (op ">>>" (v "f") (ELam "x" (v "x"))))
  , ("op: user operators are infixl 9", is "a <+> b <+> c * d" (op "*" (op "<+>" (op "<+>" (v "a") (v "b")) (v "c")) (v "d")))
  , ("op: fixities come from the session",
      runWith (Map.insert "<+>" (Fixity InfixR 1) defaultFixities) AllBound (stripLocations . desugar <$> (expr <* eof)) "a <+> b + c"
        `matches` op "<+>" (v "a") (op "+" (v "b") (v "c")))
  , ("op: reserved tokens are not operators", all (\t -> run operatorName t == Nothing) ["=", "->", "<-", "|", "&", "::", ":", "--", "-->"])
  , ("op: operator characters", run operatorName "<+>" == Just "<+>" && run operatorName "!?@^~:" == Just "!?@^~:")
  , ("minus: spaced or not, it subtracts", is "x-1" (op "-" (v "x") (int 1)) && is "x - 1" (op "-" (v "x") (int 1)))
  , ("minus: after a space, before a digit, a literal", is "f -1" (EApp (v "f") (int (-1))))
  , ("minus: in parentheses", is "(-1)" (int (-1)))
  , ("section: (- 1) subtracts one", is "(- 1)" (ELam "%1" (op "-" (v "%1") (int 1))))
  , ("section: right", is "(== 1)" (ELam "%1" (op "==" (v "%1") (int 1))))
  , ("section: left", is "(1 <)" (EApp (v "<") (int 1)) && is "(1 - )" (EApp (v "-") (int 1)))
  , ("section: an operator as a value", is "(+)" (v "+") && is "f (<>) x" (EApp (EApp (v "f") (v "<>")) (v "x")))
  , ("section: accessor", is "(.size > 1)" (ELam "%1" (op ">" (ESelect (v "%1") "size") (int 1))))
  , ("section: a nested accessor", is "(.a.b == 2 && x)" (ELam "%1" (op "&&" (op "==" (ESelect (ESelect (v "%1") "a") "b") (int 2)) (v "x"))))
  , ("op: let binds an operator", is "let (<+>) = f in 1 <+> 2" (ELet "<+>" (v "f") (op "<+>" (int 1) (int 2))))
  , ("op: a block binds an operator", is "do (%%) = f; x %% y end" (ELet "%%" (v "f") (ERun (op "%%" (v "x") (v "y")))))
  , ("op: externals keep operator characters", parseExprIn (BoundNames Set.empty) "git log -n 1 a+b" == Right (ECmd "git" (map AWord ["log", "-n", "1", "a+b"])))
  , ("op: an external ends at &&", parseExprIn (BoundNames (Set.fromList ["y"])) "make && y" == Right (op "&&" (ECmd "make" []) (v "y")))
  , ("type: functions to the right", run typeExpr "a -> b -> Int" == Just (STFun (STVar "a") (STFun (STVar "b") (STCon "Int"))))
  , ("type: lists, unit and parentheses", run typeExpr "[a] -> (a -> ()) -> String" == Just (STFun (STList (STVar "a")) (STFun (STFun (STVar "a") (STCon "()")) (STCon "String"))))
  , ("type: records, open and closed", run typeExpr "{name : String | r}" == Just (STRecord [("name", STCon "String")] (Just "r")) && run typeExpr "{}" == Just (STRecord [] Nothing))
  , ("type: applied constructors", run typeExpr "Stage a [a] -> Comp Int" == Just (STFun (STApp "Stage" Nothing [STVar "a", STList (STVar "a")]) (STApp "Comp" Nothing [STCon "Int"])))
  , ("type: a row", run typeExpr "Comp {exec git, console | e} a" == Just (STApp "Comp" (Just (SRow ["exec git", "console"] (Just "e"))) [STVar "a"]))
  , ("type: an empty row", run typeExpr "Comp {} ()" == Just (STApp "Comp" (Just (SRow [] Nothing)) [STCon "()"]))
  , ("type: a tail only", run typeExpr "Job {| e} a" == Just (STApp "Job" (Just (SRow [] (Just "e"))) [STVar "a"]))
  , ("type: a record, not a row", run typeExpr "Comp {name : String}" == Just (STApp "Comp" Nothing [STRecord [("name", STCon "String")] Nothing]))
  , ("type: an open record, not a row", run typeExpr "Comp {| r}" == Just (STApp "Comp" Nothing [STRecord [] (Just "r")]))
  , ("type: a stream's row", run typeExpr "Stream {fs} Bytes a ()" == Just (STApp "Stream" (Just (SRow ["fs"] Nothing)) [STCon "Bytes", STVar "a", STCon "()"]))
  , ("if: then and else", is "if c then a else b" (EIf (v "c") (v "a") (v "b")))
  , ("if: else goes right", is "if c then a else b | f" (EIf (v "c") (v "a") (EPipe (v "b") (v "f"))))
  , ("if: nested in else", is "if a then b else if c then d else e" (EIf (v "a") (v "b") (EIf (v "c") (v "d") (v "e"))))
  , ("if: a command stops at then and else", parseExprIn (BoundNames Set.empty) "if test -f x then echo y else echo n"
      == Right (EIf (ECmd "test" [AWord "-f", AWord "x"]) (ECmd "echo" [AWord "y"]) (ECmd "echo" [AWord "n"])))
  , ("if: else keeps its words", parseExprIn (BoundNames Set.empty) "if a then b else echo then" == Right (EIf (ECmd "a" []) (ECmd "b" []) (ECmd "echo" [AWord "then"])))
  , ("if: the words are reserved", all (\w -> run identifier w == Nothing) ["if", "then", "else"])
  , ("if: needs both branches", bad "if c then a")
  , ("decl: a class", run declaration "class Pretty a where pretty : a -> String" == Just (DeclClass [] "Pretty" "a" [("pretty", STFun (STVar "a") (STCon "String"))] []))
  , ("decl: superclasses and a block", run declaration "class (Eq a, Show a) => H a where do h : a -> Int; (<+>) : a -> a -> a end"
      == Just (DeclClass [SPred "Eq" (STVar "a"), SPred "Show" (STVar "a")] "H" "a" [("h", STFun (STVar "a") (STCon "Int")), ("<+>", STFun (STVar "a") (STFun (STVar "a") (STVar "a")))] []))
  , ("decl: an instance with a context", fmap strip (run declaration "instance Show a => Show [a] where show = xs -> \"l\"")
      == Just (DeclInstance' [SPred "Show" (STVar "a")] "Show" (STList (STVar "a")) [("show", ELam "xs" (ELit (LStr "l")))]))
  , ("decl: methods in a block", fmap strip (run declaration "instance Shape Int where do area = n -> n; name = n -> \"sq\"; end")
      == Just (DeclInstance' [] "Shape" (STCon "Int") [("area", ELam "n" (v "n")), ("name", ELam "n" (ELit (LStr "sq")))]))
  , ("decl: a function head", fmap strip (run declaration "instance Tag (a -> b) where tag = f -> 1") == Just (DeclInstance' [] "Tag" (STFun (STVar "a") (STVar "b")) [("tag", ELam "f" (int 1))]))
  , ("decl: class needs a capital", run declaration "class pretty a where p : a" == Nothing)
  ]
 where
  strip (DeclInstance ctx cls ty defs) = DeclInstance' ctx cls ty [(m, stripLocations (desugar e)) | (m, e) <- defs]
  strip (DeclClass a b c d e) = DeclClass' a b c d [(m, stripLocations (desugar x)) | (m, x) <- e]

data Strip = DeclInstance' [SPred] Name SType [(Name, Expr)] | DeclClass' [SPred] Name Name [(Name, SType)] [(Name, Expr)]
  deriving (Eq, Show)

op :: Name -> Expr -> Expr -> Expr
op name l r = EApp (EApp (v name) l) r

matches :: Either e Expr -> Expr -> Bool
matches (Right e) expected = e == expected
matches (Left _) _ = False

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
