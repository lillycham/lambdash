-- | Inference tests. Unknown names depend on the order of 'fresh', so results
-- are renamed to first-seen order before comparison.
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (unless)
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
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

prelude :: Env
prelude =
  Map.fromList
    [ ("not", TBool ~> TBool)
    , ("add", TInt ~> TInt ~> TInt)
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
  , ("applyEnv: reaches every type", applyEnv (singleton (Var 0) TInt) (Map.fromList [("x", t0), ("y", t0 ~> t1)]) == Map.fromList [("x", TInt), ("y", TInt ~> t1)])
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
