-- | A small harness with no dependencies, so the flake stays simple. Each test
-- runs on its own, so one 'undefined' does not hide the others.
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (unless)
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.List (sortOn)
import Lambdash.Unify
import System.Exit (exitFailure)
import System.Timeout (timeout)

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

r, s :: Row
r = RVar v5
s = RVar v6

v5, v6 :: Var
v5 = Var 5
v6 = Var 6

ext :: Label -> Ty -> Row -> Row
ext = RExtend

-- | Run a unifier with a counter above every unknown in these tests.
run :: Unifier a -> Either TypeError a
run u = evalStateT u 100

solve :: Ty -> Ty -> Either TypeError Subst
solve a b = run (unify a b)

-- | The fields in label order, and the tail. Rows that differ only in field
-- order have the same value here.
canon :: Row -> ([(Label, Ty)], Row)
canon (RExtend l t rest) = let (fs, tl) = canon rest in (sortOn fst ((l, t) : fs), tl)
canon tl = ([], tl)

-- | Unify two rows, then check that the substitution makes them the same.
agrees :: Row -> Row -> Bool
agrees a b = case run (unifyRow a b) of
  Right sub -> canon (applyRow sub a) == canon (applyRow sub b)
  Left _ -> False

missing :: Either TypeError a -> Bool
missing (Left (MissingField _ _)) = True
missing _ = False

infiniteRow :: Either TypeError a -> Bool
infiniteRow (Left (InfiniteRow _ _)) = True
infiniteRow _ = False

-- | Unify, then check that the substitution really makes the two types equal.
solves :: Ty -> Ty -> Bool
solves a b = case solve a b of
  Right s -> apply s a == apply s b
  Left _ -> False

-- | Unify, then check what one type became.
solvesTo :: Ty -> Ty -> Ty -> Ty -> Bool
solvesTo a b t expected = case solve a b of
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
  , ("unify: same concrete types", solve TInt TInt == Right emptySubst)
  , ("unify: every concrete type equals itself", solve TBool TBool == Right emptySubst)
  , ("unify: different concrete types", solve TInt TBool == Left (Mismatch TInt TBool))
  , ("unify: a function is not an Int", solve (TInt ~> TInt) TInt == Left (Mismatch (TInt ~> TInt) TInt))
  , ("unify: unknown on the left", solvesTo t0 TInt t0 TInt)
  , ("unify: unknown on the right", solvesTo TInt t0 t0 TInt)
  , ("unify: two unknowns", solves t0 t1)
  , ("unify: inside functions", solvesTo (t0 ~> TBool) (TInt ~> t1) (t0 ~> t1) (TInt ~> TBool))
  , ("unify: the left result reaches the right", solvesTo (t0 ~> t0) (TInt ~> t1) t1 TInt)
  , ("unify: a chain of unknowns", solvesTo (t0 ~> t1 ~> t2) (t1 ~> t2 ~> TInt) t0 TInt)
  , ("unify: conflict through an unknown", solve (t0 ~> t0) (TInt ~> TBool) == Left (Mismatch TInt TBool))
  , ("unify: x x is an infinite type", solve t0 (t0 ~> t1) == Left (Infinite v0 (t0 ~> t1)))
  , ("unify: infinite type through an unknown", refusesInfinite (solve (t0 ~> t0) (t1 ~> (t1 ~> TInt))))
  , ("applyRow: replaces the tail", applyRow (singletonRow v5 (ext "a" TInt REmpty)) (ext "b" TBool r) == ext "b" TBool (ext "a" TInt REmpty))
  , ("applyRow: goes into field types", applyRow (singleton v0 TInt) (ext "a" t0 r) == ext "a" TInt r)
  , ("applyRow: a type solution does not touch a tail", applyRow (singleton v5 TInt) r == r)
  , ("apply: goes into a record", apply (singletonRow v5 REmpty) (TRecord r) == TRecord REmpty)
  , ("compose: s1 reaches rows in s2", apply (compose (singleton v0 TInt) (singletonRow v5 (ext "a" t0 REmpty))) (TRecord r) == TRecord (ext "a" TInt REmpty))
  , ("compose: s1 reaches tails in s2", apply (compose (singletonRow v6 REmpty) (singletonRow v5 s)) (TRecord r) == TRecord REmpty)
  , ("compose: s1 rows reach types in s2", apply (compose (singletonRow v5 REmpty) (singleton v0 (TRecord r))) t0 == TRecord REmpty)
  , ("compose: s1 rows still apply alone", applyRow (compose (singletonRow v5 REmpty) (singleton v0 TInt)) r == REmpty)
  , ("occursRow: in the tail", occursRow v5 (ext "a" TInt r))
  , ("occursRow: in a field", occursRow v0 (ext "a" t0 r))
  , ("occursRow: not in a closed row", not (occursRow v5 (ext "a" TInt REmpty)))
  , ("occurs: inside a record", occurs v5 (TRecord r))
  , ("bindRow: solves an unknown", bindRow v5 REmpty == Right (singletonRow v5 REmpty))
  , ("bindRow: an unknown equals itself", bindRow v5 r == Right emptySubst)
  , ("bindRow: refuses an infinite row", bindRow v5 (ext "a" TInt r) == Left (InfiniteRow v5 (ext "a" TInt r)))
  , ("rewriteRow: field at the front", run (rewriteRow "a" (ext "a" TInt (ext "b" TBool REmpty))) == Right (TInt, ext "b" TBool REmpty, emptySubst))
  , ("rewriteRow: keeps the order of the rest", run (rewriteRow "a" (ext "x" TInt (ext "a" TBool (ext "y" TInt r)))) == Right (TBool, ext "x" TInt (ext "y" TInt r), emptySubst))
  , ("rewriteRow: no such field", missing (run (rewriteRow "a" (ext "b" TBool REmpty))))
  , ("rewriteRow: grows an unknown tail", case run (rewriteRow "a" (ext "b" TBool r)) of
        Right (t, ext', sub) -> case ext' of
          RExtend "b" TBool (RVar p) -> p /= v5 && applyRow sub r == RExtend "a" t (RVar p) && t /= t0
          _ -> False
        Left _ -> False)
  , ("rows: field order does not matter", agrees (ext "a" TInt (ext "b" TBool REmpty)) (ext "b" TBool (ext "a" TInt REmpty)))
  , ("rows: your first example", agrees (ext "name" TInt r) (ext "age" TBool (ext "name" t0 REmpty)))
  , ("rows: your first example solves t0", solvesTo (TRecord (ext "name" TInt r)) (TRecord (ext "age" TBool (ext "name" t0 REmpty))) t0 TInt)
  , ("rows: your second example", agrees (ext "name" TInt r) (ext "age" t0 s))
  , ("rows: distinct open fields share a fresh tail", case run (unifyRow (ext "a" TInt r) (ext "b" TBool s)) of
        Right sub -> case (applyRow sub r, applyRow sub s) of
          (RExtend "b" TBool (RVar leftTail), RExtend "a" TInt (RVar rightTail)) ->
            leftTail == rightTail && leftTail /= v5 && leftTail /= v6
          _ -> False
        Left _ -> False)
  , ("rewriteRow: tail growth reaches skipped field types", case run (rewriteRow "a" (ext "b" (TRecord r) r)) of
        Right (fieldTy, rest, sub) -> case applyRow sub r of
          grown@(RExtend "a" _ tailRow) ->
            rest == ext "b" (TRecord grown) tailRow
              && canon (applyRow sub (ext "b" (TRecord r) r)) == canon (ext "a" fieldTy rest)
          _ -> False
        Left _ -> False)
  , ("rows: an unknown row on the left", agrees r (ext "a" TInt REmpty))
  , ("rows: an unknown row on the right", agrees (ext "a" TInt REmpty) r)
  , ("rows: the same tail, reordered", agrees (ext "a" TInt (ext "b" TBool r)) (ext "b" TBool (ext "a" TInt r)))
  , ("rows: fields conflict", solve (TRecord (ext "a" TInt REmpty)) (TRecord (ext "a" TBool REmpty)) == Left (Mismatch TInt TBool))
  , ("rows: what one field learns reaches the next", solve (TRecord (ext "a" t0 (ext "b" t0 REmpty))) (TRecord (ext "b" TBool (ext "a" TInt REmpty))) == Left (Mismatch TInt TBool))
  , ("rows: a field is missing on the right", missing (solve (TRecord (ext "a" TInt REmpty)) (TRecord REmpty)))
  , ("rows: a field is missing on the left", missing (solve (TRecord REmpty) (TRecord (ext "a" TInt REmpty))))
  , ("rows: a closed row has no extra fields", missing (solve (TRecord (ext "a" TInt REmpty)) (TRecord (ext "a" TInt (ext "b" TBool REmpty)))))
  , ("rows: a record is not an Int", solve (TRecord REmpty) TInt == Left (Mismatch (TRecord REmpty) TInt))
  , ("rows: a solved row reaches the other side", solve (TFun (TRecord r) (TRecord r)) (TFun (TRecord (ext "a" TInt REmpty)) (TRecord (ext "a" TBool REmpty))) == Left (Mismatch TInt TBool))
  , ("rows: a row containing itself", infiniteRow (solve (TRecord r) (TRecord (ext "a" TInt r))))
  , ("rows: the same tail with other fields", infiniteRow (solve (TRecord (ext "a" TInt r)) (TRecord (ext "b" TBool r))))
  , ("rows: a grown tail reaches the field", refusesInfinite (solve (TRecord (ext "a" (TRecord r) (ext "b" TBool REmpty))) (TRecord (ext "b" TBool r))))
  ]

refusesInfinite :: Either TypeError Subst -> Bool
refusesInfinite (Left (Infinite _ _)) = True
refusesInfinite _ = False

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
