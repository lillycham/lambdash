-- | Inference tests. Unknown names depend on the order of 'fresh', so results
-- are renamed to first-seen order before comparison.
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (unless)
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Lambdash.Infer
import Lambdash.Classes qualified as Classes
import Control.Monad.Trans.Reader (runReaderT)
import Lambdash.Unify
import System.Exit (exitFailure)
import System.Timeout (timeout)

(~>) :: Ty -> Ty -> Ty
(~>) = TFun
infixr 5 ~>

t0, t1, t2 :: Ty
t0 = TVar (Var 0)
t1 = TVar (Var 1)
t2 = TVar (Var 2)

vars :: Ty -> [Var]
vars (TVar v) = [v]
vars (TFun a b) = vars a <> vars b
vars (TRecord r) = varsRow r
vars _ = []

varsRow :: Row -> [Var]
varsRow REmpty = []
varsRow (RVar x) = [x]
varsRow (RExtend _ t r) = vars t <> varsRow r

normalise :: Ty -> Ty
normalise t = go t
 where
  names = zip (nub (vars t)) (map Var [0 ..])
  go (TVar v) = TVar (fromJust (lookup v names))
  go (TFun a b) = TFun (go a) (go b)
  go (TRecord r) = TRecord (goRow r)
  go other = other
  goRow REmpty = REmpty
  goRow (RVar x) = RVar (fromJust (lookup x names))
  goRow (RExtend l a r) = RExtend l (go a) (goRow r)

lam :: [Name] -> Expr -> Expr
lam names body = foldr ELam body names

app :: Expr -> [Expr] -> Expr
app = foldl EApp

v :: Name -> Expr
v = EVar

-- | An Int. A bare literal is any Num type, and these tests are about
-- inference, not about numbers, so they pin it down.
int :: Int -> Expr
int = EApp (EVar "toInt") . ELit . LInt

true :: Expr
true = ELit (LBool True)

-- | @\\a b -> a@, to use two expressions side by side without pairs.
first :: Expr -> Expr -> Expr
first a b = app (lam ["a", "b"] (v "a")) [a, b]

-- | @\\x -> x@.
idE :: Expr
idE = lam ["x"] (v "x")

scheme :: Int -> Scheme -> Either TypeError (Ty, Ty)
scheme start sc = evalStateT ((,) <$> instantiate sc <*> instantiate sc) start

prelude :: Env
prelude =
  Map.fromList
    [ ("not", mono (TBool ~> TBool))
    , ("add", mono (TInt ~> TInt ~> TInt))
    , ("toInt", mono (TInt ~> TInt))
    ]

-- | Infer with the prelude in scope, and compare up to renaming.
typed :: Expr -> Ty -> Bool
typed expr expected = fmap normalise (inferIn prelude expr) == Right expected

mismatch :: Expr -> Bool
mismatch expr = case inferIn prelude expr of
  Left (Unify (Mismatch _ _)) -> True
  _ -> False

infinite :: Expr -> Bool
infinite expr = case inferIn prelude expr of
  Left (Unify (Infinite _ _)) -> True
  _ -> False

tests :: [(String, Bool)]
tests =
  [ ("fresh: counts up", runReaderT (evalStateT ((,) <$> fresh <*> fresh) 0) Classes.prelude == Right (t0, t1))
  , ("ftv: an unknown", ftv t0 == Set.fromList [Var 0])
  , ("ftv: both sides of a function", ftv (t0 ~> TInt ~> t1) == Set.fromList [Var 0, Var 1])
  , ("ftv: nothing in a concrete type", ftv (TInt ~> TBool) == Set.empty)
  , ("ftvScheme: leaves out bound unknowns", ftvScheme (Forall [Var 0] (t0 ~> t1)) == Set.fromList [Var 1])
  , ("ftvEnv: every entry", ftvEnv (Map.fromList [("x", mono t0), ("y", Forall [Var 1] (t1 ~> t2))]) == Set.fromList [Var 0, Var 2])
  , ("applyScheme: replaces a free unknown", applyScheme (singleton (Var 1) TInt) (Forall [Var 0] (t0 ~> t1)) == Forall [Var 0] (t0 ~> TInt))
  , ("applyScheme: leaves a bound unknown", applyScheme (singleton (Var 0) TInt) (Forall [Var 0] (t0 ~> t1)) == Forall [Var 0] (t0 ~> t1))
  , ("applyEnv: reaches every type", applyEnv (singleton (Var 0) TInt) (Map.fromList [("x", mono t0), ("y", mono (t0 ~> t1))]) == Map.fromList [("x", mono TInt), ("y", mono (TInt ~> t1))])
  , ("generalise: an empty environment binds all", generalise Map.empty (t0 ~> t1) == Forall [Var 0, Var 1] (t0 ~> t1))
  , ("generalise: keeps what the environment mentions", generalise (Map.fromList [("x", mono t0)]) (t0 ~> t1) == Forall [Var 1] (t0 ~> t1))
  , ("generalise: a concrete type binds nothing", generalise Map.empty TInt == Forall [] TInt)
  , ("instantiate: nothing bound, nothing changes", scheme 5 (mono (t0 ~> t1)) == Right (t0 ~> t1, t0 ~> t1))
  , ("instantiate: free unknowns stay", fmap (\(a, _) -> Set.member (Var 1) (ftv a)) (scheme 5 (Forall [Var 0] (t0 ~> t1))) == Right True)
  , ("instantiate: each use is its own copy", case scheme 5 (Forall [Var 0] (t0 ~> t0)) of
        Right (TFun a b, TFun c d) -> a == b && c == d && a /= c && a /= t0 && c /= t0
        _ -> False)
  , ("lit: int", typed (int 1) TInt)
  , ("lit: a bare literal is any Num type", fmap normalise (inferIn prelude (ELit (LInt 1))) == Right (TClass [IsIn "Num" t0] t0))
  , ("lit: bool", typed (ELit (LBool True)) TBool)
  , ("var: from the environment", typed (v "not") (TBool ~> TBool))
  , ("var: unbound", inferType (v "x") == Left (Unbound "x"))
  , ("lam: identity", typed (lam ["x"] (v "x")) (t0 ~> t0))
  , ("lam: const", typed (lam ["x", "y"] (v "x")) (t0 ~> t1 ~> t0))
  , ("lam: shadowing uses the inner x", typed (lam ["x", "x"] (v "x")) (t0 ~> t1 ~> t1))
  , ("app: identity on an int", typed (EApp (lam ["x"] (v "x")) (int 1)) TInt)
  , ("app: prelude function", typed (EApp (v "not") (ELit (LBool True))) TBool)
  , ("app: partial application", typed (EApp (v "add") (int 1)) (TInt ~> TInt))
  , ("app: argument type flows to a lambda", typed (lam ["x"] (EApp (v "not") (v "x"))) (TBool ~> TBool))
  , ("app: apply", typed (lam ["f", "x"] (EApp (v "f") (v "x"))) ((t0 ~> t1) ~> t0 ~> t1))
  , ("app: twice", typed (lam ["f", "x"] (EApp (v "f") (EApp (v "f") (v "x")))) ((t0 ~> t0) ~> t0 ~> t0))
  , ("app: compose", typed (lam ["f", "g", "x"] (EApp (v "f") (EApp (v "g") (v "x")))) ((t0 ~> t1) ~> (t2 ~> t0) ~> t2 ~> t1))
  , ("app: S combinator", typed (lam ["f", "g", "x"] (app (v "f") [v "x", EApp (v "g") (v "x")])) ((t0 ~> t1 ~> t2) ~> (t0 ~> t1) ~> t0 ~> t2))
  , ("error: applying an int", mismatch (EApp (int 1) (int 2)))
  , ("error: wrong argument", mismatch (EApp (v "not") (int 1)))
  , ("error: x x", infinite (lam ["x"] (EApp (v "x") (v "x"))))
  , ("error: knowledge reaches later arguments", mismatch (lam ["x"] (app (v "add") [EApp (v "not") (v "x"), v "x"])))
  , ("error: knowledge reaches the environment", mismatch (lam ["x"] (EApp (lam ["y"] (EApp (v "not") (v "x"))) (EApp (v "add") (v "x")))))
  , ("let: names a value", typed (ELet "y" (int 1) (v "y")) TInt)
  , ("let: i i", typed (ELet "i" idE (EApp (v "i") (v "i"))) (t0 ~> t0))
  , ("let: i at two types", typed (ELet "i" idE (first (EApp (v "i") (int 1)) (EApp (v "i") true))) TInt)
  , ("let: an inner let shadows", typed (ELet "x" (int 1) (ELet "x" true (v "x"))) TBool)
  , ("let: shadows a lambda parameter", typed (lam ["x"] (ELet "x" (int 1) (v "x"))) (t0 ~> TInt))
  , ("let: learning from the binding reaches the result", typed (lam ["x"] (ELet "y" (EApp (v "not") (v "x")) (v "x"))) (TBool ~> TBool))
  , ("let: learning from the body reaches the result", typed (lam ["x"] (ELet "y" (int 1) (EApp (v "not") (v "x")))) (TBool ~> TBool))
  , ("let: generalises some unknowns and keeps others", typed (lam ["x"] (ELet "k" (lam ["y"] (v "x")) (first (EApp (v "k") (int 1)) (EApp (v "k") true)))) (t0 ~> t0))
  , ("lam: does not generalise, so i i fails", infinite (EApp (lam ["i"] (EApp (v "i") (v "i"))) idE))
  , ("error: an unused binding is still checked", mismatch (ELet "y" (EApp (v "not") (int 1)) (int 3)))
  , ("error: let is not recursive", inferIn prelude (ELet "f" (v "f") (int 1)) == Left (Unbound "f"))
  , ("let rec: a function sees itself", typed (ELetRec [("f", lam ["x"] (EApp (v "f") (v "x")))] (v "f")) (t0 ~> t1))
  , ("let rec: what the body learns reaches the type", typed (ELetRec [("f", lam ["x"] (EApp (v "not") (EApp (v "f") (v "x"))))] (v "f")) (t0 ~> TBool))
  , ("let rec: generalises after the group", typed (ELetRec [("i", idE)] (first (EApp (v "i") (int 1)) (EApp (v "i") true))) TInt)
  , ("let rec: one type inside the group", mismatch (ELetRec [("g", lam ["x"] (first (EApp (v "g") (int 1)) (EApp (v "g") true)))] (v "g")))
  , ("let rec: mutual", typed (ELetRec [("e", lam ["n"] (EApp (v "o") (v "n"))), ("o", lam ["n"] (EApp (v "not") (EApp (v "e") (v "n"))))] (v "e")) (t0 ~> TBool))
  , ("let rec: members share the group's knowledge", typed (ELetRec [("e", lam ["n"] (EApp (v "o") (v "n"))), ("o", lam ["n"] (app (v "add") [v "n", EApp (v "e") (v "n")]))] (v "e")) (TInt ~> TInt))
  , ("let rec: the names are not bound outside", inferIn prelude (ELet "y" (ELetRec [("f", idE)] (int 1)) (v "f")) == Left (Unbound "f"))
  , ("error: learning from the binding reaches the body", mismatch (lam ["x"] (ELet "y" (EApp (v "not") (v "x")) (app (v "add") [v "x", int 1]))))
  , ("error: the binding of a parameter stays one type", mismatch (lam ["x"] (ELet "y" (v "x") (first (app (v "add") [v "y", int 1]) (EApp (v "not") (v "y"))))))
  , ("error: the result of a parameter stays one type", mismatch (lam ["x"] (ELet "y" (EApp (v "x") (int 1)) (first (app (v "add") [v "y", int 1]) (EApp (v "not") (v "y"))))))
  , ("error: a kept unknown stays one type", mismatch (lam ["x"] (ELet "k" (lam ["y"] (v "x")) (first (app (v "add") [EApp (v "k") (int 1), int 1]) (EApp (v "not") (EApp (v "k") true))))))
  , ("ftv: inside a computation", ftv ((TComp noEffects) t0) == Set.fromList [Var 0])
  , ("lit: unit", typed (ELit LUnit) TUnit)
  , ("lit: string", typed (ELit (LStr "a")) TString)
  , ("external: a computation", typed (ECmd "ls" []) (TStream (effect (Exec "ls")) TBytes TBytes TStatus))
  , ("external: a parameter becomes a String", typed (lam ["x"] (ECmd "ls" [AVar "x"])) (TString ~> TStream (effect (Exec "ls")) TBytes TBytes TStatus))
  , ("commands: under a lambda", commands (lam ["x"] (ECmd "ls" [])) == ["ls"])
  , ("commands: in every part", commands (ERecord [("a", ECmd "a" []), ("b", EApp (ECmd "b" []) (ELet "x" (ECmd "c" []) (ECmd "d" [])))]) == ["a", "b", "c", "d"])
  , ("inferRunIn: gives the result type", inferRunIn (Map.fromList [("c", mono ((TComp noEffects) TInt))]) (v "c") == Right TInt)
  , ("inferRunIn: refuses a non-computation", case inferRunIn prelude (int 1) of
      Left (Unify (Mismatch TInt (TComp _ (TVar _)))) -> True
      _ -> False)
  , ("inferRunIn: refuses a function", case inferRunIn prelude (v "not") of
        Left (Unify (Mismatch _ _)) -> True
        _ -> False)
  , ("ftvRow: fields and tail", ftvRow (RExtend "a" t0 (RVar (Var 1))) == Set.fromList [Var 0, Var 1])
  , ("ftv: inside a record", ftv (TRecord (RVar (Var 1))) == Set.fromList [Var 1])
  , ("applyScheme: leaves a bound row unknown", applyScheme (singletonRow (Var 1) REmpty) (Forall [Var 1] (TRecord (RVar (Var 1)))) == Forall [Var 1] (TRecord (RVar (Var 1))))
  , ("generalise: binds a row unknown", generalise Map.empty (TRecord (RVar (Var 1))) == Forall [Var 1] (TRecord (RVar (Var 1))))
  , ("instantiate: a row unknown is its own copy", case scheme 5 (Forall [Var 1] (TRecord (RVar (Var 1)))) of
        Right (TRecord (RVar a), TRecord (RVar b)) -> a /= b && a /= Var 1 && b /= Var 1
        _ -> False)
  , ("record: empty", typed (ERecord []) (TRecord REmpty))
  , ("record: fields", typed (ERecord [("n", int 1), ("ok", true)]) (TRecord (RExtend "n" TInt (RExtend "ok" TBool REmpty))))
  , ("record: a field's learning reaches the next field", typed (lam ["x"] (ERecord [("a", EApp (v "not") (v "x")), ("b", v "x")])) (TBool ~> TRecord (RExtend "a" TBool (RExtend "b" TBool REmpty))))
  , ("select: from a record", typed (ESelect (ERecord [("n", int 1), ("ok", true)]) "ok") TBool)
  , ("select: any record with the field", typed (lam ["r"] (ESelect (v "r") "name")) (TRecord (RExtend "name" t0 (RVar (Var 1))) ~> t0))
  , ("select: two fields of one parameter", typed (EApp (lam ["r"] (first (ESelect (v "r") "a") (ESelect (v "r") "b"))) (ERecord [("b", true), ("a", int 1)])) TInt)
  , ("select: let makes a selector polymorphic", typed (ELet "name" (lam ["r"] (ESelect (v "r") "name")) (first (EApp (v "name") (ERecord [("name", int 1)])) (EApp (v "name") (ERecord [("x", int 2), ("name", true)])))) TInt)
  , ("error: no such field", case inferIn prelude (ESelect (ERecord [("name", int 1)]) "nmae") of
        Left (Unify (MissingField "nmae" _)) -> True
        _ -> False)
  , ("error: a parameter's field stays one type", mismatch (lam ["r"] (first (app (v "add") [ESelect (v "r") "a", int 1]) (EApp (v "not") (ESelect (v "r") "a")))))
  , ("error: a row in the environment stays one row", mismatch (lam ["r"] (ELet "s" (v "r") (first (app (v "add") [ESelect (v "s") "a", int 1]) (EApp (v "not") (ESelect (v "s") "a"))))))
  , ("error: a field's learning reaches the next field", mismatch (lam ["x"] (ERecord [("a", EApp (v "not") (v "x")), ("b", app (v "add") [v "x", int 1])])))
  , ("record: later constraints reach earlier fields", typed (lam ["x"] (ERecord [("a", v "x"), ("b", EApp (v "not") (v "x"))])) (TBool ~> TRecord (RExtend "a" TBool (RExtend "b" TBool REmpty))))
  , ("select: nested records", typed (ESelect (ESelect (ERecord [("outer", ERecord [("inner", int 1)])]) "outer") "inner") TInt)
  , ("select: a concrete value is not a record", mismatch (ESelect (int 1) "a"))
  , ("let: a record's function field is polymorphic", typed (ELet "r" (ERecord [("id", idE)]) (first (EApp (ESelect (v "r") "id") (int 1)) (EApp (ESelect (v "r") "id") true))) TInt)
  , ("instantiate: type and row copies stay linked", case scheme 5 (Forall [Var 0, Var 1] (TRecord (RExtend "a" t0 (RVar (Var 1))) ~> t0)) of
        Right (TFun (TRecord (RExtend "a" a (RVar ra))) b, TFun (TRecord (RExtend "a" c (RVar rc))) d) ->
          a == b && c == d && a /= c && ra /= rc && a /= TVar ra && c /= TVar rc
        _ -> False)
  ]

runTest :: (String, Bool) -> IO Bool
runTest (name, result) = do
  -- The time limit is for row unification, which can loop forever.
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
