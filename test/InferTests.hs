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
import Lambdash.Unify
import System.Exit (exitFailure)

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
vars _ = []

normalise :: Ty -> Ty
normalise t = go t
 where
  names = zip (nub (vars t)) (map Var [0 ..])
  go (TVar v) = TVar (fromJust (lookup v names))
  go (TFun a b) = TFun (go a) (go b)
  go other = other

lam :: [Name] -> Expr -> Expr
lam names body = foldr ELam body names

app :: Expr -> [Expr] -> Expr
app = foldl EApp

v :: Name -> Expr
v = EVar

int :: Int -> Expr
int = ELit . LInt

true :: Expr
true = ELit (LBool True)

-- | @\\a b -> a@, to use two expressions side by side without pairs.
first :: Expr -> Expr -> Expr
first a b = app (lam ["a", "b"] (v "a")) [a, b]

-- | @\\x -> x@.
idE :: Expr
idE = lam ["x"] (v "x")

scheme :: Int -> Scheme -> Either InferError (Ty, Ty)
scheme start sc = evalStateT ((,) <$> instantiate sc <*> instantiate sc) start

prelude :: Env
prelude =
  Map.fromList
    [ ("not", mono (TBool ~> TBool))
    , ("add", mono (TInt ~> TInt ~> TInt))
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
  [ ("fresh: counts up", evalStateT ((,) <$> fresh <*> fresh) 0 == Right (t0, t1))
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
  , ("error: learning from the binding reaches the body", mismatch (lam ["x"] (ELet "y" (EApp (v "not") (v "x")) (app (v "add") [v "x", int 1]))))
  , ("error: the binding of a parameter stays one type", mismatch (lam ["x"] (ELet "y" (v "x") (first (app (v "add") [v "y", int 1]) (EApp (v "not") (v "y"))))))
  , ("error: the result of a parameter stays one type", mismatch (lam ["x"] (ELet "y" (EApp (v "x") (int 1)) (first (app (v "add") [v "y", int 1]) (EApp (v "not") (v "y"))))))
  , ("error: a kept unknown stays one type", mismatch (lam ["x"] (ELet "k" (lam ["y"] (v "x")) (first (app (v "add") [EApp (v "k") (int 1), int 1]) (EApp (v "not") (EApp (v "k") true))))))
  ]

runTest :: (String, Bool) -> IO Bool
runTest (name, result) = do
  outcome <- try (evaluate result) :: IO (Either SomeException Bool)
  let (passed, label) = case outcome of
        Right True -> (True, "pass")
        Right False -> (False, "FAIL")
        Left _ -> (False, "TODO")
  putStrLn (label <> "  " <> name)
  pure passed

main :: IO ()
main = do
  results <- mapM runTest tests
  putStrLn (show (length (filter id results)) <> "/" <> show (length results) <> " passed")
  unless (and results) exitFailure
