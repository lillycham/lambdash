-- | A small harness with no dependencies, so the flake stays simple. Each test
-- runs on its own, so one 'undefined' does not hide the others.
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (unless)
import Lambdash.Unify
import System.Exit (exitFailure)

t0, t1, t2 :: Ty
t0 = TVar v0
t1 = TVar (Var 1)
t2 = TVar (Var 2)

v0, v1 :: Var
v0 = Var 0
v1 = Var 1

(~>) :: Ty -> Ty -> Ty
(~>) = TFun
infixr 5 ~>

-- | Unify, then check that the substitution really makes the two types equal.
solves :: Ty -> Ty -> Bool
solves a b = case unify a b of
  Right s -> apply s a == apply s b
  Left _ -> False

-- | Unify, then check what one type became.
solvesTo :: Ty -> Ty -> Ty -> Ty -> Bool
solvesTo a b t expected = case unify a b of
  Right s -> apply s t == expected
  Left _ -> False

tests :: [(String, Bool)]
tests =
  [ ("apply: empty changes nothing", apply emptySubst (t0 ~> TInt) == t0 ~> TInt)
  , ("apply: replaces an unknown", apply (singleton v0 TInt) t0 == TInt)
  , ("apply: leaves other unknowns", apply (singleton v0 TInt) t1 == t1)
  , ("apply: goes under TFun", apply (singleton v0 TInt) (t0 ~> t0) == TInt ~> TInt)
  , ("compose: s2 happens first", apply (compose (singleton v1 TInt) (singleton v0 t1)) t0 == TInt)
  , ("compose: s1 still applies alone", apply (compose (singleton v1 TInt) (singleton v0 t1)) t1 == TInt)
  , ("compose: s2 wins on the same unknown", apply (compose (singleton v0 TInt) (singleton v0 TBool)) t0 == TBool)
  , ("occurs: finds itself", occurs v0 t0)
  , ("occurs: does not find another", not (occurs v0 t1))
  , ("occurs: finds inside TFun", occurs v0 (t1 ~> TInt ~> t0))
  , ("occurs: not in a concrete type", not (occurs v0 (TInt ~> TBool)))
  , ("bind: solves an unknown", bind v0 TInt == Right (singleton v0 TInt))
  , ("bind: an unknown equals itself", bind v0 t0 == Right emptySubst)
  , ("bind: refuses an infinite type", bind v0 (t0 ~> t1) == Left (Infinite v0 (t0 ~> t1)))
  , ("unify: same concrete types", unify TInt TInt == Right emptySubst)
  , ("unify: different concrete types", unify TInt TBool == Left (Mismatch TInt TBool))
  , ("unify: a function is not an Int", unify (TInt ~> TInt) TInt == Left (Mismatch (TInt ~> TInt) TInt))
  , ("unify: unknown on the left", solvesTo t0 TInt t0 TInt)
  , ("unify: unknown on the right", solvesTo TInt t0 t0 TInt)
  , ("unify: two unknowns", solves t0 t1)
  , ("unify: inside functions", solvesTo (t0 ~> TBool) (TInt ~> t1) (t0 ~> t1) (TInt ~> TBool))
  , ("unify: the left result reaches the right", solvesTo (t0 ~> t0) (TInt ~> t1) t1 TInt)
  , ("unify: a chain of unknowns", solvesTo (t0 ~> t1 ~> t2) (t1 ~> t2 ~> TInt) t0 TInt)
  , ("unify: conflict through an unknown", unify (t0 ~> t0) (TInt ~> TBool) == Left (Mismatch TInt TBool))
  , ("unify: x x is an infinite type", unify t0 (t0 ~> t1) == Left (Infinite v0 (t0 ~> t1)))
  , ("unify: infinite type through an unknown", refusesInfinite (unify (t0 ~> t0) (t1 ~> (t1 ~> TInt))))
  ]

refusesInfinite :: Either TypeError Subst -> Bool
refusesInfinite (Left (Infinite _ _)) = True
refusesInfinite _ = False

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
