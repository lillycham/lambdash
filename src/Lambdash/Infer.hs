-- | Type inference for a small core: variables, lambdas, application, @let@.
--
-- This is Algorithm W. Each step returns what it learned as a
-- substitution, together with the type it found.
module Lambdash.Infer
  ( Name
  , Lit (..)
  , Arg (..)
  , Expr (..)
  , Span (..)
  , commands
  , Scheme (..)
  , mono
  , Env
  , InferError (..)
  , Infer
  , fresh
  , unifying
  , ftv
  , ftvRow
  , ftvEffects
  , actionEffects
  , ftvScheme
  , ftvEnv
  , applyScheme
  , applyEnv
  , generalise
  , instantiate
  , infer
  , inferIn
  , inferRunIn
  , inferType
  , Mode (..)
  , elaborateIn
  , elaborateRunIn
  , inferWith
  , inferRunWith
  , elaborateWith
  , elaborateRunWith
  , elaborateInstance
  , elaborateSigned
  ) where

import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, mapStateT, put, runStateT)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (lift)
import Control.Monad (foldM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Lambdash.Classes
import Lambdash.TypeSyntax (writtenType)
import Lambdash.Syntax (Arg (..), Expr (..), Lit (..), Name, SPred, SType, Span (..))
import Lambdash.Unify
import qualified Data.Set as Set


-- | A type that holds for every choice of the listed unknowns.
-- @Forall [a] (a -> a)@ is @forall a. a -> a@. The other unknowns are free.
data Scheme = Forall [Var] Ty
  deriving (Eq, Show)

-- | A type with nothing to choose. Lambda parameters have this.
mono :: Ty -> Scheme
mono = Forall []

-- | What each name in scope has as its type.
type Env = Map Name Scheme

data InferError
  = -- | Two types did not unify.
    Unify TypeError
  | -- | A name that nothing binds.
    Unbound Name
  | -- | A class predicate that no instance satisfies.
    NoInstance Pred
  | -- | A class predicate on an unknown that nothing chooses, and that
    -- @()@ does not satisfy either.
    Ambiguous Pred
  | -- | A class or instance declaration that cannot stand.
    BadDeclaration String
  | -- | An instance needs this, and neither an instance nor its context
    -- gives it.
    Unentailed Pred
  | -- | A binding needs this, and its signature's context does not give it.
    SignatureNeeds Pred
  deriving (Eq, Show)

-- | The state is the next unused unknown, and the classes and instances in
-- scope are read-only. Failure is 'InferError': see 'failWith'.
type Infer = StateT Int (ReaderT ClassEnv (Either InferError))

failWith :: InferError -> Infer a
failWith = lift . lift . Left

classEnv :: Infer ClassEnv
classEnv = lift ask

-- | Make an unknown that nothing has used yet.
fresh :: Infer Ty
fresh = do
  n <- get
  put (n + 1)
  pure (TVar (Var n))

-- | Run unification in here.
unifying :: Unifier a -> Infer a
unifying = mapStateT (lift . either (Left . Unify) Right)

-- | The unknowns that appear in a type.
--
-- Note: this has to see row unknowns as well, or @generalise@ misses them.
ftv :: Ty -> Set Var
ftv (TVar v) = Set.singleton v
ftv (TFun t1 t2) = ftv t1 `Set.union` ftv t2
ftv (TCon _) = Set.empty
ftv (TRecord r) = ftvRow r
ftv (TComp eff t) = ftvEffects eff `Set.union` ftv t
ftv (TJob eff t) = ftvEffects eff `Set.union` ftv t
ftv (TStream eff i o r) = Set.unions (ftvEffects eff : map ftv [i, o, r])
ftv (TQualified cs ty) = Set.unions (ftv ty : map constraintVars cs)
ftv (TClass ps ty) = Set.unions (ftv ty : map predVars ps)
ftv (TList ty) = ftv ty
ftv (TStage i o) = ftv i `Set.union` ftv o
ftv (TCollector i o) = ftv i `Set.union` ftv o

ftvEffects :: EffRow -> Set Var
ftvEffects (EffRow _ vars) = vars

-- | Only the outer action executes. A returned action, or one inside a
-- record, stays latent until it is run separately.
actionEffects :: Ty -> EffRow
actionEffects (TComp eff _) = eff
actionEffects (TStream eff _ _ _) = eff
actionEffects (TQualified _ ty) = actionEffects ty
actionEffects (TClass _ ty) = actionEffects ty
actionEffects _ = noEffects

-- | The unknowns that appear in a row.
ftvRow :: Row -> Set Var
ftvRow REmpty = Set.empty
ftvRow (RVar v) = Set.singleton v
ftvRow (RExtend _ t r) = ftv t `Set.union` ftvRow r

-- | The unknowns that a scheme does not bind.
ftvScheme :: Scheme -> Set Var
ftvScheme (Forall bound ty) = ftv ty `Set.difference` Set.fromList bound

-- | The unknowns free in any scheme in the environment. These are the unknowns
-- that something outside a @let@ can still constrain.
ftvEnv :: Env -> Set Var
ftvEnv = Set.unions . map ftvScheme . Map.elems

-- | Apply a substitution to the free unknowns of a scheme. A bound unknown only
-- marks a place to fill in, so the substitution must not touch it.
applyScheme :: Subst -> Scheme -> Scheme
applyScheme sub (Forall bound ty) =
  Forall bound (apply freeSubst ty)
 where
  boundVars = Set.fromList bound
  freeSubst = Subst
    { getSubst = Map.withoutKeys (getSubst sub) boundVars
    , getRowSubst = Map.withoutKeys (getRowSubst sub) boundVars
    , getEffectSubst = Map.withoutKeys (getEffectSubst sub) boundVars
    , effectConstraints = []
    , classPreds = []
    }

-- | Apply a substitution to every scheme in the environment.
applyEnv :: Subst -> Env -> Env
applyEnv s e = Map.map (applyScheme s) e

-- | Bind each unknown of the type that the environment does not mention, in
-- ascending order.
--
-- Note: the caller has to apply what it learned to the environment first.
generalise :: Env -> Ty -> Scheme
generalise env t = Forall as qualified
 where
  (constraints, bare) = unqualify t
  relevant = connectedConstraints (ftv bare `Set.union` ftvEnv env) constraints
  fixed = constraintClosure (ftvEnv env) relevant
  qualified = qualify relevant bare
  as = Set.toList (ftv qualified `Set.difference` fixed)

constraintClosure :: Set Var -> [EffConstraint] -> Set Var
constraintClosure seed constraints =
  let next = Set.unions (seed : [constraintVars c | c <- constraints,
        not (Set.null (Set.intersection seed (constraintVars c)))])
  in if next == seed then seed else constraintClosure next constraints

connectedConstraints :: Set Var -> [EffConstraint] -> [EffConstraint]
connectedConstraints seed constraints =
  let connected = constraintClosure seed constraints
  in [c | c <- constraints, not (Set.null (Set.intersection connected (constraintVars c)))]

-- | Replace each bound unknown with a fresh one, so that each use of a name is
-- its own copy.
instantiate :: Scheme -> Unifier Ty
instantiate (Forall bound ty) = do
  freshVars <- mapM (const freshVar) bound
  let renaming = Map.fromList (zip bound freshVars)
      sub = Subst
        { getSubst = TVar <$> renaming
        , getRowSubst = RVar <$> renaming
        , getEffectSubst = effectVar <$> renaming
        , effectConstraints = []
        , classPreds = []
        }
  pure (apply sub ty)

-- | Find the type of an expression, what that taught us, and the expression
-- elaborated: each use of a name whose scheme has class predicates is applied
-- to evidence variables, and each generalised @let@ abstracts over the
-- dictionaries its scheme needs. The predicates still to resolve travel in the
-- substitution, like deferred effect equalities.
--
-- Note: the 'Subst' from each step has to reach the steps after it, in the
-- environment and in the types you already hold.
infer :: Env -> Expr -> Infer (Subst, Ty, Expr)
infer env (EVar name) =
  case Map.lookup name env of
    Nothing -> failWith ((Unbound name))
    Just sc -> do
      t <- unifying (instantiate sc)
      let (constraints, qualified) = unqualify t
          (preds, bare) = unclass qualified
      evidence <- mapM (const freshEvidence) preds
      pure ( emptySubst {effectConstraints = constraints, classPreds = zip evidence preds}
           , bare
           , foldl EApp (EVar name) (map EVar evidence) )

-- A numeric literal is any Num type: 1 is fromInt d 1.
infer _ e@(ELit (LInt _)) = do
  a <- fresh
  evidence <- freshEvidence
  pure (emptySubst {classPreds = [(evidence, IsIn "Num" a)]}, a, EApp (EApp (EVar fromIntName) (EVar evidence)) e)
infer _ e@(ELit (LBool _)) = pure (emptySubst, TBool, e)
infer _ e@(ELit LUnit)     = pure (emptySubst, TUnit, e)
infer _ e@(ELit (LStr _))  = pure (emptySubst, TString, e)
infer _ e@(ELit (LRef _))  = pure (emptySubst, TString, e)

-- For now, an argument must be a String. The checker has nothing to convert with.
infer env e@(ECmd command args) = do
  sub <- foldM check emptySubst args
  pure (sub, TStream (effect (Exec command)) TBytes TBytes TStatus, e)
 where
  check known (AWord _) = pure known
  check known (AVar name) = do
    (s, t, _) <- infer (applyEnv known env) (EVar name)
    checked <- unifying (unify (apply s t) TString)
    pure (checked `compose` s `compose` known)

infer env (ELam name body) = do
  tv <- fresh
  let env' = Map.insert name (mono tv) env
  (s, t, body') <- infer env' body
  pure (s, apply s (TFun tv t), ELam name body')

infer env (EApp e1 e2) = do
  (s1, t1, e1') <- infer env e1
  (s2, t2, e2') <- infer (applyEnv s1 env) e2
  tv <- fresh
  let t1' = apply s2 t1
  s3 <- unifying (unify t1' (TFun t2 tv))
  let s_final = s3 `compose` s2 `compose` s1
  pure (s_final, apply s3 tv, EApp e1' e2')

infer env (EPipe left right) = do
  (s1, leftTy, left'') <- infer env left
  (s2, rightTy, right'') <- infer (applyEnv s1 env) right
  -- An unknown producer is a stream. Deferring this choice to evaluation
  -- would let a polymorphic function change meaning after instantiation.
  -- An unknown that a class constrains is a value, as 1 in 1 | add 2 is:
  -- no stream is an instance of a class.
  let constrainedVars = Set.unions [predVars (applyPred (s2 `compose` s1) p) | (_, p) <- classPreds (s2 `compose` s1)]
  shape <- case apply s2 leftTy of
    TVar v | Set.notMember v constrainedVars -> do
      input <- fresh
      output <- fresh
      result <- fresh
      eff <- unifying (effectVar <$> freshVar)
      unifying (unify (apply s2 leftTy) (TStream eff input output result))
    _ -> pure emptySubst
  let known = shape `compose` s2 `compose` s1
      left' = apply shape (apply s2 leftTy)
      right' = apply shape rightTy
  (s3, result) <- case (left', right') of
    (TStream eff input output result', TCollector item collected) -> do
      a <- unifying (unify output item)
      b <- unifying (unify (apply a input) TBytes)
      pure (b `compose` a, TComp eff (collectedTy collected result'))
    (TStage input output, TCollector item collected) -> do
      s <- unifying (unify output item)
      pure (s, TCollector input collected)
    (TComp eff value, TCollector item collected) -> do
      s <- unifying (unify value item)
      pure (s, TComp eff (collectedTy collected TUnit))
    (_, TCollector item collected) -> do
      s <- unifying (unify left' item)
      pure (s, TComp noEffects (collectedTy collected TUnit))
    (TStream eff input output result', TStage item output') -> do
      s <- unifying (unify output item)
      pure (s, TStream eff input output' result')
    (TStage input output, TStage item output') -> do
      s <- unifying (unify output item)
      pure (s, TStage input output')
    (TComp eff value, TStage input output) -> do
      s <- unifying (unify value input)
      pure (s, TStream eff TBytes output TUnit)
    (_, TStage input output) -> do
      s <- unifying (unify left' input)
      pure (s, TStream noEffects TBytes output TUnit)
    (TStage input output, _) -> do
      item <- fresh
      s <- unifying (unify right' (TFun output item))
      pure (s, TStage input item)
    (TStream eff input output _, TStream eff' input' output' result') -> do
      s <- unifying (unify output input')
      pure (s, TStream (unionEffects eff eff') input output' result')
    (TStream eff input output result', _) -> do
      item <- fresh
      s <- unifying (unify right' (TFun output item))
      pure (s, TStream eff input item result')
    (TComp eff value, TStream eff' input output result') -> do
      s <- unifying (unify value input)
      pure (s, TStream (unionEffects eff eff') TBytes output result')
    (_, TStream eff input output result') -> do
      s <- unifying (unify left' input)
      pure (s, TStream eff TBytes output result')
    (TComp eff value, _) -> do
      item <- fresh
      s <- unifying (unify right' (TFun value item))
      pure (s, TComp eff item)
    _ -> do
      item <- fresh
      s <- unifying (unify right' (TFun left' item))
      pure (s, item)
  pure (s3 `compose` known, apply s3 result, EPipe left'' right'')
 where
  collectedTy item result = TRecord (RExtend "items" (TList item) (RExtend "status" result (RExtend "statuses" (TList TStatus) REmpty)))

infer env (EList items) = do
  itemTy <- fresh
  (sub, items') <- foldM (check itemTy) (emptySubst, []) items
  pure (sub, TList (apply sub itemTy), EList (reverse items'))
 where
  check itemTy (known, done) item = do
    (learned, ty, item') <- infer (applyEnv known env) item
    let combined = learned `compose` known
    equal <- unifying (unify (apply combined itemTy) ty)
    pure (equal `compose` combined, item' : done)

infer env (ELet x e1 e2) = do
  (raw, t1, e1') <- infer env e1
  s1 <- checkedConstraints raw
  let env' = applyEnv s1 env
      -- Constraints involving an enclosing monomorphic variable must escape
      -- even when the local binding is unused. Independent quantified rows
      -- belong to the scheme and must not leak between its instances.
      escaped = connectedConstraints (ftvEnv env') (effectConstraints s1)
  (sc, e1'', deferred) <- generaliseBinding env' s1 t1 e1'
  (s2, t2, e2') <- infer (Map.insert x sc env') e2
  pure (s2 `compose` s1 {effectConstraints = escaped, classPreds = deferred}, apply s2 t2, ELet x e1'' e2')

infer env (ERecord fields) = do
  (sub, row, fields') <- inferFields env fields
  pure (sub, TRecord row, ERecord fields')
 where
  inferFields :: Env -> [(Label, Expr)] -> Infer (Subst, Row, [(Label, Expr)])
  inferFields _ [] = pure (emptySubst, REmpty, [])
  inferFields fieldEnv ((label, expr) : rest) = do
    (fieldSub, fieldTy, expr') <- infer fieldEnv expr
    (restSub, restRow, rest') <- inferFields (applyEnv fieldSub fieldEnv) rest
    pure (restSub `compose` fieldSub, RExtend label (apply restSub fieldTy) restRow, (label, expr') : rest')

infer env (ESelect expr label) = do
  (exprSub, recordTy, expr') <- infer env expr
  fieldTy <- fresh
  tailRow <- unifying (RVar <$> freshVar)
  let requiredTy = TRecord (RExtend label fieldTy tailRow)
  fieldSub <- unifying (unify recordTy requiredTy)
  pure (fieldSub `compose` exprSub, apply fieldSub fieldTy, ESelect expr' label)

-- An annotation's unknowns are fresh, so it narrows the type and no more:
-- (1 : Int) is an Int, and (f : a -> a) any f that fits.
infer env (EAnn e written) = do
  let (bound, annotated) = writtenType [] written
  t <- unifying (instantiate (Forall bound annotated))
  (s1, te, e') <- infer env e
  s2 <- fitting (apply s1 te) (apply s1 t)
  pure (s2 `compose` s1, apply s2 (apply s1 t), e')

-- A location changes nothing about the type.
infer env (ELoc location e) = do
  (s, t, e') <- infer env e
  pure (s, t, ELoc location e')

-- Run the first, then the rest, with the union of their rows. Like a lambda
-- parameter, a bound result has one type in what follows.
infer env (EBind name e body) = do
  (s1, eff, value, e') <- inferRun env e
  let env' = Map.insert name (mono value) (applyEnv s1 env)
  (s2, bodyTy, body') <- infer env' body
  rest <- unifying (effectVar <$> freshVar)
  result <- fresh
  s3 <- unifying (unify bodyTy (TComp rest result))
  let sub = s3 `compose` s2 `compose` s1
  pure (sub, TComp (unionEffects (applyEffects sub eff) (applyEffects sub rest)) (apply sub result), EBind name e' body')

infer env (ERun e) = do
  (s, eff, result, e') <- inferRun env e
  pure (s, TComp eff result, ERun e')

-- The checker drops the exec labels it can see in the body: the body runs
-- with the overlay, so the session's PATH says nothing about them. Each
-- pipeline still resolves its programs before it spawns anything. A label
-- hidden in an unknown tail, such as a parameter's, stays.
infer env (EWith overlay body) = do
  (s1, overlayTy, overlay') <- infer env overlay
  s2 <- unifying (unify overlayTy TOverlay)
  let known = s2 `compose` s1
  (s3, bodyTy, body') <- infer (applyEnv known env) body
  let sub = s3 `compose` known
      code = EWith overlay' body'
  case apply sub bodyTy of
    TComp (EffRow labels tails) result ->
      pure (sub, TComp (EffRow (Set.filter (not . isExec) labels) tails) result, code)
    other -> do
      eff <- unifying (effectVar <$> freshVar)
      result <- fresh
      s4 <- unifying (unify other (TComp eff result))
      pure (s4 `compose` sub, apply s4 result, code)
 where
  isExec (Exec _) = True
  isExec _ = False

-- Starting a job starts its commands, so its row is the job's row, and the
-- preflight checks them before anything runs. The row also travels in the
-- handle, for wait and fg.
infer env (EBg e) = do
  (s, eff, result, e') <- inferRun env e
  pure (s, TComp eff (TJob eff result), EBg e')

-- | Unify a type with the one it must fit, where a closed row in the one
-- expected is a ceiling: Comp {} () fits Comp {console} (), as doing less
-- than allowed is fine. Rows are not values, so widening one needs no
-- conversion. Elsewhere, and where either row is open, this is 'unify'.
-- Only results widen: a function's parameter must match exactly.
fitting :: Ty -> Ty -> Infer Subst
fitting actual expected = case (actual, expected) of
  (TFun a b, TFun a' b') -> do
    s1 <- unifying (unify a a')
    s2 <- fitting (apply s1 b) (apply s1 b')
    pure (s2 `compose` s1)
  (TComp r t, TComp r' t') -> inside r r' t t'
  (TJob r t, TJob r' t') -> inside r r' t t'
  (TStream r i o x, TStream r' i' o' x') -> do
    s1 <- unifying (unify (TFun i o) (TFun i' o'))
    s2 <- row (applyEffects s1 r) (applyEffects s1 r')
    let known = s2 `compose` s1
    s3 <- fitting (apply known x) (apply known x')
    pure (s3 `compose` known)
  _ -> unifying (unify actual expected)
 where
  inside r r' t t' = do
    s1 <- row r r'
    s2 <- fitting (apply s1 t) (apply s1 t')
    pure (s2 `compose` s1)
  row a@(EffRow labels tails) b@(EffRow allowed tails')
    | Set.null tails && Set.null tails' =
        if labels `Set.isSubsetOf` allowed then pure emptySubst else failWith (Unify (EffectMismatch a b))
    | otherwise = unifying (unify (TComp a TUnit) (TComp b TUnit))

-- | What running an expression requires and gives. A computation gives its
-- result. A stream reads the terminal, so its input is Bytes, and gives its
-- return value, as at the prompt.
inferRun :: Env -> Expr -> Infer (Subst, EffRow, Ty, Expr)
inferRun env e = do
  (s1, t, e') <- infer env e
  case apply s1 t of
    TStream eff input _ result -> do
      s2 <- unifying (unify input TBytes)
      pure (s2 `compose` s1, applyEffects s2 eff, apply s2 result, e')
    TComp eff result -> pure (s1, eff, result, e')
    other -> do
      eff <- unifying (effectVar <$> freshVar)
      result <- fresh
      s2 <- unifying (unify other (TComp eff result))
      pure (s2 `compose` s1, applyEffects s2 eff, apply s2 result, e')

-- | A name for the dictionary of one predicate. No identifier can spell it.
freshEvidence :: Infer Name
freshEvidence = do
  n <- get
  put (n + 1)
  pure ("%d" <> show n)

-- | Generalise a @let@ binding over the unknowns the environment does not
-- fix, and over the dictionaries its scheme needs. Predicates that mention
-- an unknown of the environment stay pending, for an enclosing binding.
generaliseBinding :: Env -> Subst -> Ty -> Expr -> Infer (Scheme, Expr, [(Name, Pred)])
generaliseBinding env s1 t1 e1 = do
  env' <- classEnv
  let Forall bound qualified = generalise env (qualify (effectConstraints s1) (apply s1 t1))
      preds = [(n, applyPred s1 p) | (n, p) <- classPreds s1]
  (bindings, residual) <- simplify preds
  let -- The monomorphism restriction: a binding with no parameters does not
      -- generalise over its class predicates. Their unknowns stay unknowns,
      -- for the rest of the program to choose or for defaulting: so
      -- let n = 1 in add n 2 is fine, and n is one value, not one per use.
      restricted = not (isLambda e1)
      constrained = Set.unions (map (predVars . snd) residual)
      bound' = if restricted then filter (`Set.notMember` constrained) bound else bound
      generalised = Set.fromList bound'
      ownedBy (_, p) = not restricted && let vs = predVars p in not (Set.null vs) && vs `Set.isSubsetOf` generalised
      mine = filter ownedBy residual
      deferred = filter (not . ownedBy) residual
      (context, reduced) = reduceContext env' mine
      scheme = Forall bound' (if null context then qualified else TClass (map snd context) qualified)
  -- Reduced evidence comes first: an instance's dictionary can refer to it.
  pure (scheme, foldr ELam (wrapLets (reduced <> bindings) e1) (map fst context), deferred)

-- | Resolve predicates on constructed types from instances, and keep the
-- ones on unknowns. A resolved predicate's evidence is the instance's
-- dictionary, applied to the evidence for what the instance requires.
-- Bindings come in dependency order.
simplify :: [(Name, Pred)] -> Infer ([(Name, Expr)], [(Name, Pred)])
simplify [] = pure ([], [])
simplify ((evidence, IsIn cls ty) : rest) = do
  (bindings, residual) <- simplify rest
  env <- classEnv
  case ty of
    TVar _ -> pure (bindings, (evidence, IsIn cls ty) : residual)
    -- A record is an instance when every field is, and an open tail is a
    -- predicate on the tail: Eq {a : Int | r} needs Eq Int and Eq { | r}.
    TRecord row -> case lookupInstance env cls "Record" of
      Nothing -> failWith ((NoInstance (IsIn cls ty)))
      Just _ -> case flatten row of
        ([], RVar _) -> pure (bindings, (evidence, IsIn cls ty) : residual)
        (fields, tailRow) -> do
          fieldEvidence <- mapM (\(label, fieldTy) -> (\n -> (label, n, IsIn cls fieldTy)) <$> freshEvidence) fields
          tailEvidence <- case tailRow of
            RVar v -> (\n -> [(n, IsIn cls (TRecord (RVar v)))]) <$> freshEvidence
            _ -> pure []
          (inner, innerResidual) <- simplify ([(n, p) | (_, n, p) <- fieldEvidence] <> tailEvidence)
          let known = ERecord [(label, EVar n) | (label, n, _) <- fieldEvidence]
              base = case tailEvidence of
                [(n, _)] -> ESelect (EVar n) recordFields
                _ -> ERecord []
              dictionary = EApp (EVar (instanceName cls "Record")) (EApp (EApp (EVar extendFields) known) base)
          pure (inner <> [(evidence, dictionary)] <> bindings, innerResidual <> residual)
    _ -> case tyKey ty of
      Nothing -> failWith ((NoInstance (IsIn cls ty)))
      Just (key, args) -> case lookupInstance env cls key of
        Nothing -> failWith ((NoInstance (IsIn cls ty)))
        Just inst -> do
          required <- sequence
            [ (\n -> (n, IsIn c arg)) <$> freshEvidence
            | (arg, needs) <- zip args (instRequires inst), c <- needs ]
          (inner, innerResidual) <- simplify required
          let dictionary = foldl EApp (EVar (instanceName cls key)) [EVar n | (n, _) <- required]
          pure (inner <> [(evidence, dictionary)] <> bindings, innerResidual <> residual)

-- | Drop a predicate that another one provides: a duplicate, or a
-- superclass, whose dictionary is a field of its subclass's dictionary.
reduceContext :: ClassEnv -> [(Name, Pred)] -> ([(Name, Pred)], [(Name, Expr)])
reduceContext env preds = go [] [] preds
 where
  go kept bindings [] = (reverse kept, bindings)
  go kept bindings ((evidence, p@(IsIn cls ty)) : rest) =
    case [n | (n, q) <- kept <> rest, q == p, n /= evidence, n `elem` map fst kept] of
      same : _ -> go kept (bindings <> [(evidence, EVar same)]) rest
      [] -> case [ (n, sub) | (n, IsIn sub ty') <- kept <> rest, ty' == ty, cls `elem` superclassesOf env sub ] of
        (n, sub) : _ -> go kept (bindings <> [(evidence, superPath env sub cls (EVar n))]) rest
        [] -> go ((evidence, p) : kept) bindings rest

-- | The dictionary of a superclass, found through the fields of a subclass's.
superPath :: ClassEnv -> String -> String -> Expr -> Expr
superPath env from to dictionary
  | from == to = dictionary
  | otherwise = case [s | ClassInfo {className = name, classSupers = supers} <- envClasses env, name == from, s <- supers, s == to || to `elem` superclassesOf env s] of
      next : _ -> superPath env next to (ESelect dictionary (superField next))
      [] -> dictionary

-- | Whether an expression is a function as written: the monomorphism
-- restriction applies to the others.
isLambda :: Expr -> Bool
isLambda (ELoc _ e) = isLambda e
isLambda (ELam _ _) = True
isLambda _ = False

-- | A row's fields, in order, and its tail.
flatten :: Row -> ([(Label, Ty)], Row)
flatten (RExtend label ty rest) = let (fields, tailRow) = flatten rest in ((label, ty) : fields, tailRow)
flatten tailRow = ([], tailRow)

wrapLets :: [(Name, Expr)] -> Expr -> Expr
wrapLets bindings e = foldr (\(n, d) body -> ELet n d body) e bindings

-- | What to do with class predicates left at the top of a line.
data Mode
  = -- | For @:t@: show the ones the type mentions.
    Showing
  | -- | For @x = e@: abstract over the ones the type mentions.
    Generalising
  | -- | For a line that runs: default every unknown to @()@.
    Defaulting
  deriving (Eq, Show)

-- | Resolve the class predicates of a whole line. An unknown that a
-- predicate mentions and the type does not is ambiguous, and defaults to
-- @()@, as GHCi's extended defaulting does: @eq [] []@ is @true@.
resolveClasses :: Mode -> Subst -> Ty -> Expr -> Infer (Ty, Expr)
resolveClasses requested solved ty code = do
  -- x = e without parameters is restricted: it defaults instead.
  let mode = if requested == Generalising && not (isLambda code) then Defaulting else requested
  let preds = [(n, applyPred solved p) | (n, p) <- classPreds solved]
  (bindings, residual) <- simplify preds
  env <- classEnv
  let inType = ftv ty
      keeps (_, p) = mode /= Defaulting && predVars p `Set.isSubsetOf` inType
      (context, reduced) = reduceContext env (filter keeps residual)
      ambiguous = filter (not . keeps) residual
  (defaulted, chosen) <- defaultPreds ambiguous
  -- Defaulted and reduced evidence comes first: an instance's dictionary,
  -- such as Eq [a]'s, can refer to the evidence for its argument. What
  -- defaulting chose is part of the type: x = 1 is x : Int.
  let lets = wrapLets (defaulted <> reduced <> bindings) code
      ty' = apply chosen ty
      shown = if null context then ty' else TClass (map snd context) ty'
  pure (shown, foldr ELam lets (map fst context))

-- | Default each unknown to the first candidate that every class on it
-- accepts, and each unknown row to the empty row, and resolve again.
defaultPreds :: [(Name, Pred)] -> Infer ([(Name, Expr)], Subst)
defaultPreds preds = do
  env <- classEnv
  let vars = Set.toList (Set.unions [predVars p | (_, p) <- preds])
      classesOn v = [cls | (_, IsIn cls (TVar v')) <- preds, v' == v]
      accepts t cls = case tyKey t of
        Just (key, _) -> maybe False (const True) (lookupInstance env cls key)
        Nothing -> False
      choose v = case [t | t <- defaultCandidates, all (accepts t) (classesOn v)] of
        t : _ -> t
        [] -> TUnit
      fill = Subst (Map.fromList [(v, choose v) | v <- vars]) (Map.fromList [(v, REmpty) | v <- vars]) Map.empty [] []
      defaulted = [(n, applyPred fill p) | (n, p) <- preds]
  (bindings, residual) <- simplify defaulted `orElse` preds
  case residual of
    [] -> pure (bindings, fill)
    (_, p) : _ -> failWith ((Ambiguous p))
 where
  -- A candidate that fails gives an ambiguity, not a missing instance.
  orElse action original = do
    state <- get
    env <- classEnv
    case runReaderT (runStateT action state) env of
      Left (NoInstance _) -> case original of
        (_, p) : _ -> failWith ((Ambiguous p))
        [] -> pure ([], [])
      Left err -> failWith (err)
      Right (result, next) -> put next >> pure result

-- | The external commands that an expression names, also under lambdas.
commands :: Expr -> [Name]
commands (ECmd name _) = [name]
commands (ELam _ body) = commands body
commands (EApp f x) = commands f <> commands x
commands (EPipe left right) = commands left <> commands right
commands (ELet _ bound body) = commands bound <> commands body
commands (ERecord fields) = concatMap (commands . snd) fields
commands (EList items) = concatMap commands items
commands (ESelect record _) = commands record
commands (EBind _ e body) = commands e <> commands body
commands (ERun e) = commands e
commands (ELoc _ e) = commands e
commands (EWith overlay body) = commands overlay <> commands body
commands (EBg e) = commands e
commands (EAnn e _) = commands e
commands _ = []

-- | Infer in a starting environment, with the final solution applied. Class
-- predicates on unknowns that the type mentions appear as its context.
inferIn :: Env -> Expr -> Either InferError Ty
inferIn = inferWith prelude

inferWith :: ClassEnv -> Env -> Expr -> Either InferError Ty
inferWith classes env expr = fst <$> elaborateWith classes Showing env expr

-- | The type of a line, and the line elaborated with its dictionaries.
elaborateIn :: Mode -> Env -> Expr -> Either InferError (Ty, Expr)
elaborateIn = elaborateWith prelude

elaborateWith :: ClassEnv -> Mode -> Env -> Expr -> Either InferError (Ty, Expr)
elaborateWith classes mode env expr = runReaderT (evalStateT go (nextUnknown env)) classes
 where
  go = do
    (s, t, code) <- infer env expr
    (solved, ty) <- finish env s t
    resolveClasses mode solved ty code

checkedConstraints :: Subst -> Infer Subst
checkedConstraints = lift . lift . either (Left . Unify) Right . solveConstraints

finish :: Env -> Subst -> Ty -> Infer (Subst, Ty)
finish env raw ty = do
  let (nested, bare) = unqualify ty
  solved <- checkedConstraints raw {effectConstraints = effectConstraints raw <> nested}
  let result = apply solved bare
      fixed = ftvEnv (applyEnv solved env)
      relevant = connectedConstraints (ftv result `Set.union` fixed) (effectConstraints solved)
  pure (solved, qualify relevant result)

-- | The return type of a computation or stream. A stream's output items are
-- separate from its return value. An unknown action defaults to @TComp a@.
inferRunIn :: Env -> Expr -> Either InferError Ty
inferRunIn = inferRunWith prelude

inferRunWith :: ClassEnv -> Env -> Expr -> Either InferError Ty
inferRunWith classes env expr = (\(_, result, _) -> result) <$> elaborateRunWith classes env expr

-- | For @x <- e@: the type of the action, the type of what running it gives,
-- and the action elaborated, with its class predicates defaulted.
elaborateRunIn :: Env -> Expr -> Either InferError (Ty, Ty, Expr)
elaborateRunIn = elaborateRunWith prelude

elaborateRunWith :: ClassEnv -> Env -> Expr -> Either InferError (Ty, Ty, Expr)
elaborateRunWith classes env expr = runReaderT (evalStateT go (nextUnknown env)) classes
 where
  go = do
    (s1, t, code) <- infer env expr
    (s2, resultTy) <- case apply s1 t of
      TStream _ input _ result -> do
        s <- unifying (unify input TBytes)
        pure (s `compose` s1, result)
      TComp _ result -> pure (s1, result)
      ty -> do
        result <- fresh
        eff <- case ty of
          TVar _ -> unifying (effectVar <$> freshVar)
          _ -> pure noEffects
        s <- unifying (unify ty (TComp eff result))
        pure (s `compose` s1, result)
    (solved, actionTy) <- finish env s2 t
    (_, result) <- finish env s2 resultTy
    -- Defaulting chooses for the action and its result alike.
    (both, code') <- resolveClasses Defaulting solved (TFun actionTy result) code
    pure $ case both of
      TFun actionTy' result' -> (actionTy', result', code')
      _ -> (actionTy, result, code')

-- | Check an instance declaration, and elaborate its dictionary: a record of
-- its methods and of its superclasses' dictionaries, under a lambda for each
-- dictionary its context requires. The class environment already holds the
-- instance, so a method can use the instance itself, as a list's method uses
-- the list instance on the elements' lists.
--
-- The head's unknowns are rigid: a method may not need them to be anything
-- in particular, and a method's own unknowns stay as general as its class
-- says. Every predicate a method needs must follow from an instance or from
-- the context, as must each superclass of the head.
elaborateInstance :: ClassEnv -> Env -> Instance -> Ty -> [(String, Expr)] -> Either InferError Expr
elaborateInstance classes env inst headTy definitions = runReaderT (evalStateT go start) classes
 where
  cls = instClass inst
  start = maximum (nextUnknown env : [n + 1 | Var n <- Set.toList (ftv headTy)])
  headArgs = maybe [] snd (tyKey headTy)
  go = do
    info <- maybe (failWith (BadDeclaration ("no class " <> cls))) pure (lookupClass classes cls)
    let wanted = map fst (classMethods info)
        given = map fst definitions
        problem = case ([m | m <- wanted, m `notElem` given], [m | m <- given, m `notElem` wanted], duplicates given) of
          (m : _, _, _) -> Just ("the instance has no " <> m)
          (_, m : _, _) -> Just (m <> " is not a method of " <> cls)
          (_, _, m : _) -> Just (m <> " is defined twice")
          _ -> Nothing
    maybe (pure ()) (failWith . BadDeclaration) problem
    -- Each method's type at the head, with its own unknowns fresh.
    expected <- mapM (\(m, mty) -> do
      let atHead = apply emptySubst {getSubst = Map.singleton (classVar info) headTy} mty
          own = Set.toList (Set.delete (classVar info) (ftv mty))
      t <- unifying (instantiate (Forall own atHead))
      pure (m, t)) (classMethods info)
    let ordered = [(m, e) | (m, _) <- expected, Just e <- [lookup m definitions]]
    -- Each method fits its type: a closed row there is a ceiling.
    (learned, fields') <- foldM (\(known, done) (m, e) -> do
      (s, t, e') <- infer (applyEnv known env) e
      let sofar = s `compose` known
      fit <- fitting (apply sofar t) (apply sofar (maybe t id (lookup m expected)))
      pure (fit `compose` sofar, done <> [(m, e')])) (emptySubst, []) ordered
    let record' = ERecord fields'
    solved <- checkedConstraints learned
    -- Rigid: the head's unknowns and each method's own must stay distinct
    -- unknowns.
    let rigid = Set.toList (Set.unions (ftv headTy : [ftv t | (_, t) <- expected]))
        images = [apply solved (TVar v) | v <- rigid]
    if all isVar images && length (nubOrd images) == length images
      then pure ()
      else failWith (BadDeclaration "a method is less general than the class says")
    superEvidence <- mapM (\sup -> (,) sup <$> freshEvidence) (classSupers info)
    let headTy' = apply solved headTy
        preds = [(n, applyPred solved p) | (n, p) <- classPreds solved]
          <> [(n, IsIn sup headTy') | (sup, n) <- superEvidence]
    (bindings, residual) <- simplify preds
    givens <- sequence
      [ (\n -> (n, IsIn c (apply solved arg))) <$> freshEvidence
      | (arg, needs) <- zip headArgs (instRequires inst), c <- needs ]
    resolved <- mapM (\(n, p@(IsIn c t)) ->
      case [(g, c') | (g, IsIn c' t') <- givens, t' == t, c' == c || c `elem` superclassesOf classes c'] of
        (g, c') : _ -> pure (n, superPath classes c' c (EVar g))
        [] -> failWith (Unentailed p)) residual
    let fields = case record' of
          ERecord fs -> fs
          _ -> []
        dictionary = ERecord (fields <> [(superField sup, EVar n) | (sup, n) <- superEvidence])
    pure (foldr ELam (wrapLets (resolved <> bindings) dictionary) (map fst givens))
  isVar (TVar _) = True
  isVar _ = False
  nubOrd = Set.toList . Set.fromList . map show
  duplicates names = [n | (i, n) <- zip [0 :: Int ..] names, n `elem` take i names]

-- | Check a binding against its signature, and elaborate it. The signature's
-- unknowns are rigid: the binding must work for every choice of them, so
-- it is at least as general as the signature says. Its context gives the
-- dictionaries, which the code takes as parameters in the context's order;
-- every predicate on a rigid unknown must follow from them. A signature
-- generalises a binding that the monomorphism restriction would not: with
-- n : Num a => a, n = 1 is one value per type.
--
-- Effect rows and record tails in a signature are not rigid: the binding's
-- own rows fill them in, so the preflight still sees its commands.
elaborateSigned :: ClassEnv -> Env -> ([SPred], SType) -> Expr -> Either InferError (Ty, Expr)
elaborateSigned classes env (context, written) expr = runReaderT (evalStateT go start) classes
 where
  (bound, signature) = writtenType context written
  start = maximum (nextUnknown env : [n + 1 | Var n <- bound])
  go = do
    t <- unifying (instantiate (Forall bound signature))
    let (sigPreds, sigBare) = unclass t
    (s1, te, code) <- infer env expr
    s2 <- fitting (apply s1 te) (apply s1 sigBare)
    (solved, ty) <- finish env (s2 `compose` s1) sigBare
    let rigid = Set.toList (typeUnknowns sigBare)
        images = [apply solved (TVar v) | v <- rigid]
        fixed = ftvEnv (applyEnv solved env)
        distinct = Set.size (Set.fromList [v | TVar v <- images]) == length images
    if all isVar images && distinct && not (any (`Set.member` fixed) [v | TVar v <- images])
      then pure ()
      else failWith (BadDeclaration "the binding is less general than its signature")
    givens <- mapM (\p -> (\n -> (n, applyPred solved p)) <$> freshEvidence) sigPreds
    (bindings, residual) <- simplify [(n, applyPred solved p) | (n, p) <- classPreds solved]
    env' <- classEnv
    let rigidVars = Set.fromList [v | TVar v <- images]
        onRigid (_, p) = not (Set.null (Set.intersection (predVars p) rigidVars))
    resolved <- mapM (\(n, p@(IsIn c t')) ->
      case [(g, c') | (g, IsIn c' t'') <- givens, t'' == t', c' == c || c `elem` superclassesOf env' c'] of
        (g, c') : _ -> pure (n, superPath env' c' c (EVar g))
        [] -> failWith (SignatureNeeds p)) (filter onRigid residual)
    -- What the signature leaves open, as in g : Int -> Bool with eq [] [],
    -- defaults as a line does.
    (defaulted, chosen) <- defaultPreds (filter (not . onRigid) residual)
    let final = apply chosen ty
        shown = if null givens then final else TClass (map snd givens) final
    pure (shown, foldr ELam (wrapLets (resolved <> defaulted <> bindings) code) (map fst givens))
  isVar (TVar _) = True
  isVar _ = False
  -- The unknowns in type positions: not rows, which stay flexible.
  typeUnknowns ty = case ty of
    TVar v -> Set.singleton v
    TFun a b -> Set.union (typeUnknowns a) (typeUnknowns b)
    TList a -> typeUnknowns a
    TRecord row -> rowUnknowns row
    TComp _ a -> typeUnknowns a
    TJob _ a -> typeUnknowns a
    TStream _ i o r -> Set.unions (map typeUnknowns [i, o, r])
    TStage i o -> Set.union (typeUnknowns i) (typeUnknowns o)
    TCollector i o -> Set.union (typeUnknowns i) (typeUnknowns o)
    TQualified _ a -> typeUnknowns a
    TClass _ a -> typeUnknowns a
    TCon _ -> Set.empty
  rowUnknowns (RExtend _ t r) = Set.union (typeUnknowns t) (rowUnknowns r)
  rowUnknowns _ = Set.empty

-- Include bound names too: a fresh substitution range must not capture a
-- quantified variable when that substitution reaches another scheme.
nextUnknown :: Env -> Int
nextUnknown env = 1 + maximum (-1 : [number | Forall bound ty <- Map.elems env,
  Var number <- Set.toList (Set.union (Set.fromList bound) (ftv ty))])

-- | Infer with nothing in scope.
inferType :: Expr -> Either InferError Ty
inferType = inferIn Map.empty

