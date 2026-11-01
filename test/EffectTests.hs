module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (unless)
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.List (nub, subsequences)
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

bounded :: String
bounded = "s -> ((f -> {a = f (s | cat), b = f (cat)}) (x -> x)).a"

-- Independent finite-model oracle. The extra label represents effects not
-- mentioned in a constraint, so accidental closed-world reasoning fails.
models :: [Subst]
models = [emptySubst {getEffectSubst = Map.fromList [(v0, closed a), (v1, closed b)]}
  | a <- subsets, b <- subsets]
 where
  subsets = subsequences [Exec "echo", Exec "cat", Exec "other"]
  closed labels = EffRow (Set.fromList labels) Set.empty

holds :: Subst -> [EffConstraint] -> Bool
holds model = all (\(EffEqual a b) -> applyEffects model a == applyEffects model b)

constraintCases :: [[EffConstraint]]
constraintCases = [[a, b] | a <- pool, b <- pool]
 where
  x = effectVar v0
  y = effectVar v1
  pool = [EffEqual (unionEffects echo x) echo, EffEqual (unionEffects cat x) x,
    EffEqual x y, EffEqual (unionEffects x y) echo, EffEqual (unionEffects x y) noEffects,
    EffEqual (unionEffects echo x) (unionEffects cat y), EffEqual echo cat,
    EffEqual x (unionEffects echo y), EffEqual (unionEffects echo x) (unionEffects echo y)]

preservesModels :: [EffConstraint] -> Bool
preservesModels equations = case solveConstraints emptySubst {effectConstraints = equations} of
  Left _ -> not (any (`holds` equations) models)
  Right sub -> let solved = effectConstraints sub <>
                    [EffEqual (effectVar var) row | (var, row) <- Map.toList (getEffectSubst sub)]
               in all (\model -> holds model equations == holds model solved) models

provesClosedRow :: [EffConstraint] -> EffRow -> Bool
provesClosedRow equations row = closeEffects equations row == expected
 where
  alternatives = nub [applyEffects model row | model <- models, holds model equations]
  expected = case alternatives of [one] -> Just one; _ -> Nothing

tests :: [(String, Bool)]
tests =
  [ ("constraints: SAT agrees with exhaustive finite models", all (\cs -> satisfiableEffects cs == any (`holds` cs) models) constraintCases)
  , ("constraints: substitutions preserve all finite models", all preservesModels constraintCases)
  , ("constraints: closed-row proofs agree with finite models", all (\cs -> all (provesClosedRow cs) [effectVar v0, unionEffects echo (effectVar v0), unionEffects (effectVar v0) (effectVar v1), echo]) constraintCases)
  , ("constraints: bounds do not guess a value", case solveConstraints emptySubst {effectConstraints = [EffEqual (unionEffects echo (effectVar v0)) echo]} of
        Right sub -> Map.null (getEffectSubst sub) && not (null (effectConstraints sub))
        _ -> False)
  , ("constraints: contradictory bounds fail jointly", not (satisfiableEffects [EffEqual (unionEffects echo (effectVar v0)) echo, EffEqual (unionEffects cat (effectVar v0)) (effectVar v0)]))
  , ("constraints: generalisation retains qualifications", case inferSource bounded of
        Right ty@(TQualified cs _) -> not (null cs) && case generalise Map.empty ty of
          Forall boundVars qualified -> not (null boundVars) && not (null (fst (unqualify qualified)))
        _ -> False)
  , ("constraints: a later application resolves a bound", typed ("(" <> bounded <> ") (cat /dev/null)") "Stream {exec cat} Bytes Bytes Status")
  , ("constraints: a later application rejects a contradiction", case inferSource ("(" <> bounded <> ") (printf side-effect)") of
        Left _ -> True
        _ -> False)
  , ("constraints: scheme instances have separate constraints", case inferSource bounded of
        Right ty -> case evalStateT ((,) <$> instantiate (generalise Map.empty ty) <*> instantiate (generalise Map.empty ty)) 100 of
          Right (a, b) -> Set.null (Set.intersection (ftv a) (ftv b)) && not (null (fst (unqualify a))) && not (null (fst (unqualify b)))
          _ -> False
        _ -> False)
  , ("schemes: substitution ranges cannot capture quantified names", case inferIn captureEnv captureExpr of
        Right (TFun (TVar a) (TFun (TVar b) (TVar c))) -> a == c && a /= b
        _ -> False)
  , ("constraints: argument checks preserve qualifications", case inferIn
      (Map.singleton "x" (Forall [] (qualify [EffEqual (unionEffects echo (effectVar v0)) echo] TString)))
      (ECmd "cat" [AVar "x"]) of
        Right (TQualified cs _) -> not (null cs)
        _ -> False)
  , ("constraints: structural equality does not erase nested obligations", let
        impossible = TRecord (RExtend "f" (qualify [EffEqual echo cat] (TFun TInt TInt)) REmpty)
        in case run (unify impossible impossible) >>= solveConstraints of
          Left _ -> True
          _ -> False)
  , ("rows: union is idempotent", unionEffects echo echo == echo)
  , ("rows: union is commutative", unionEffects echo cat == unionEffects cat echo)
  , ("rows: distinct tails survive union", ftvEffects (unionEffects (effectVar v0) (effectVar v1)) == Set.fromList [v0, v1])
  , ("rows: a bare unknown takes a closed row", agrees (effectVar v0) echo)
  , ("rows: a bare unknown takes an open union", agrees (effectVar v0) (unionEffects echo (effectVar v1)))
  , ("rows: a bare unknown takes independent tails", agrees (effectVar v0) (unionEffects (effectVar v1) (effectVar (Var 2))))
  , ("rows: self absorption terminates", agrees (effectVar v0) (unionEffects echo (effectVar v0)))
  , ("rows: a shared tail absorbs distinct labels", agrees (unionEffects echo (effectVar v0)) (unionEffects cat (effectVar v0)))
  , ("rows: closed effects do not silently widen", run (unifyEffects echo cat) == Left (EffectMismatch echo cat))
  , ("rows: ambiguous equality is retained", case run (unifyEffects (unionEffects echo (effectVar v0)) echo) of
        Right sub -> not (null (effectConstraints sub)) && Map.null (getEffectSubst sub)
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
  captureEnv = Map.fromList
    [("g", Forall [v1] (TFun (TVar v1) (TVar v0))), ("u", mono (TFun (TVar v0) TUnit))]
  captureExpr = ELam "x" (EApp (ELam "ignored" (EVar "g")) (EApp (EVar "u") (EVar "x")))

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
