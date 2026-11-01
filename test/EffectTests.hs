module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (unless)
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Lambdash.Eval (typeEnv)
import Lambdash.Infer
import Lambdash.Parser
import Lambdash.Repl (renderEffectsType, renderType)
import Lambdash.Unify
import System.Exit (exitFailure)
import System.Timeout (timeout)

v0, v1 :: Var
v0 = Var 0
v1 = Var 1

echo, cat :: EffRow
echo = effect (Exec "echo")
cat = effect (Exec "cat")

run :: Unifier a -> Either TypeError a
run action = evalStateT action 100

agrees :: EffRow -> EffRow -> Bool
agrees left right = case run (unifyEffects left right) of
  Left _ -> False
  Right sub -> applyEffects sub left == applyEffects sub right

inferSource :: String -> Either String Ty
inferSource source = do
  expression <- parseExprIn (BoundNames (Map.keysSet typeEnv)) source
  either (Left . show) Right (inferIn typeEnv expression)

typed :: String -> String -> Bool
typed source expected = fmap renderEffectsType (inferSource source) == Right expected

tests :: [(String, Bool)]
tests =
  [ ("rows: union is idempotent", unionEffects echo echo == echo)
  , ("rows: union is commutative", unionEffects echo cat == unionEffects cat echo)
  , ("rows: distinct tails survive union", ftvEffects (unionEffects (effectVar v0) (effectVar v1)) == Set.fromList [v0, v1])
  , ("rows: a bare unknown takes a closed row", agrees (effectVar v0) echo)
  , ("rows: a bare unknown takes an open union", agrees (effectVar v0) (unionEffects echo (effectVar v1)))
  , ("rows: a bare unknown takes independent tails", agrees (effectVar v0) (unionEffects (effectVar v1) (effectVar (Var 2))))
  , ("rows: self absorption terminates", agrees (effectVar v0) (unionEffects echo (effectVar v0)))
  , ("rows: a shared tail absorbs distinct labels", agrees (unionEffects echo (effectVar v0)) (unionEffects cat (effectVar v0)))
  , ("rows: closed effects do not silently widen", run (unifyEffects echo cat) == Left (EffectMismatch echo cat))
  , ("rows: ambiguous equality is diagnosed", case run (unifyEffects (unionEffects echo (effectVar v0)) echo) of
        Left (UnresolvedEffects _ _) -> True
        _ -> False)
  , ("substitution: effect rows compose", applyEffects (compose (singletonEffect v1 echo) (singletonEffect v0 (effectVar v1))) (effectVar v0) == echo)
  , ("substitution: computation effects substitute", apply (singletonEffect v0 echo) (TComp (effectVar v0) TInt) == TComp echo TInt)
  , ("substitution: stream effects substitute", apply (singletonEffect v0 echo) (TStream (effectVar v0) TBytes TBytes TStatus) == TStream echo TBytes TBytes TStatus)
  , ("schemes: effect unknowns are free", ftv (TFun (TComp (effectVar v0) TInt) (TComp (effectVar v0) TInt)) == Set.singleton v0)
  , ("schemes: effect unknowns generalise", generalise Map.empty (TComp (effectVar v0) TInt) == Forall [v0] (TComp (effectVar v0) TInt))
  , ("schemes: environment effects do not generalise", generalise (Map.singleton "c" (mono (TComp (effectVar v0) TInt))) (TComp (effectVar v0) TInt) == mono (TComp (effectVar v0) TInt))
  , ("schemes: bound effect unknowns resist substitution", applyScheme (singletonEffect v0 echo) (Forall [v0] (TComp (effectVar v0) TInt)) == Forall [v0] (TComp (effectVar v0) TInt))
  , ("inference: fresh unknowns avoid free environment rows", case inferIn (Map.singleton "source" (mono (TStream (effectVar v0) TBytes TBytes TStatus))) (ELam "x" (EPipe (EVar "source") (EVar "x"))) of
        Right (TFun (TFun TBytes (TVar output)) (TStream row _ (TVar output') _)) -> output == output' && output /= v0 && row == effectVar v0
        _ -> False)
  , ("schemes: instances keep independent rows", case evalStateT ((,) <$> instantiate scheme <*> instantiate scheme) 10 of
        Right (TComp a _, TComp b _) -> a /= b && not (Set.member v0 (ftvEffects a))
        _ -> False)
  , ("unification: stream effects reach nested item types", case run (unify
      (TStream (effectVar v0) TBytes (TComp (effectVar v0) TInt) TStatus)
      (TStream echo TBytes (TComp echo TInt) TStatus)) of
        Right sub -> applyEffects sub (effectVar v0) == echo
        _ -> False)
  , ("inference: external requirements", typed "echo hello" "Stream {exec echo} Bytes Bytes Status")
  , ("inference: pipeline unions requirements", typed "echo hello | cat" "Stream {exec cat, exec echo} Bytes Bytes Status")
  , ("inference: duplicate requirements collapse", typed "cat | cat" "Stream {exec cat} Bytes Bytes Status")
  , ("inference: print has a console effect", typed "print 1" "Comp {console} ()")
  , ("inference: pure has no immediate effects", typed "pure 1" "Comp Int")
  , ("inference: latent commands stay in the result", typed "pure (echo hi)" "Comp (Stream {exec echo} Bytes Bytes Status)")
  , ("inference: higher-order application preserves effects", typed "(f x -> f x) (x -> echo $x) \"hi\"" "Stream {exec echo} Bytes Bytes Status")
  , ("inference: generic rows propagate", typed "s -> s | cat" "Stream { | a} b Bytes c -> Stream {exec cat | a} b Bytes Status")
  , ("inference: independent higher-order inputs compose", case inferSource "a b -> (a | cat) | (b | cat)" of
        Right (TFun _ (TFun _ (TStream (EffRow labels tails) _ _ _))) -> labels == Set.singleton (Exec "cat") && Set.size tails == 2
        _ -> False)
  , ("inference: polymorphic comp instances stay separate", typed "let id = (x -> x) in {a = id (print 1), b = id (pure 1)}" "{a : Comp {console} (), b : Comp Int}")
  , ("inference: pure mapping keeps action effects", typed "print 1 | (x -> encodeUtf8 \"a\") | cat" "Stream {exec cat, console} Bytes Bytes Status")
  , ("inference: a mapped returned action stays latent", typed "printf hi | (x -> print x)" "Stream {exec printf} Bytes (Comp {console} ()) Status")
  , ("preflight: only outer action effects", actionEffects (TComp noEffects (TStream echo TBytes TBytes TStatus)) == noEffects)
  , ("display: normal types omit effects", renderType (TFun TString (TStream echo TBytes TBytes TStatus)) == "String -> Stream Bytes Bytes Status")
  ]
 where
  scheme = Forall [v0] (TComp (effectVar v0) TInt)

main :: IO ()
main = do
  results <- mapM check tests
  putStrLn (show (length (filter id results)) <> "/" <> show (length results) <> " passed")
  unless (and results) exitFailure
 where
  check (name, result) = do
    outcome <- try (timeout 2000000 (evaluate result)) :: IO (Either SomeException (Maybe Bool))
    let passed = case outcome of Right (Just True) -> True; _ -> False
    putStrLn ((if passed then "pass  " else "FAIL  ") <> name)
    pure passed
