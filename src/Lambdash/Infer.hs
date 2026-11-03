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
  ) where

import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, mapStateT, put, runStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad (foldM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Lambdash.Classes
import Lambdash.Syntax (Arg (..), Lit (..), Name, Span (..))
import Lambdash.Unify
import qualified Data.Set as Set

data Expr
  = EVar Name
  | ELit Lit
  | ELam Name Expr
  | EApp Expr Expr
  | EPipe Expr Expr
  | -- | @let name = bound in body@. It is not recursive: @name@ is not in scope
    -- in @bound@.
    ELet Name Expr Expr
  | -- | @git status@: a head that nothing binds, and its arguments.
    ECmd Name [Arg]
  | -- | @{name = 1, ok = true}@.
    ERecord [(Label, Expr)]
  | EList [Expr]
  | -- | @r.name@.
    ESelect Expr Label
  | -- | Run the first, name its result, then the computation that follows.
    -- A block desugars to these: @x <- e; rest@ is @EBind x e rest@.
    EBind Name Expr Expr
  | -- | Run a computation or a stream, as a block statement or at the prompt
    -- does: a stream reads the terminal and gives its return value.
    ERun Expr
  | -- | Where an expression came from. It means the expression.
    ELoc Span Expr
  | -- | @with overlay do ... end@: run the block with one more environment
    -- layer, for its dynamic extent only.
    EWith Expr Expr
  | -- | @e &@: start @e@ as a background job, and give its handle.
    EBg Expr
  deriving (Eq, Show)

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
  deriving (Eq, Show)

-- | The state is the next unused unknown. Failure is 'InferError'. Use @lift@
-- from "Control.Monad.Trans.Class" to run an @Either InferError a@ in here.
type Infer = StateT Int (Either InferError)

-- | Make an unknown that nothing has used yet.
fresh :: Infer Ty
fresh = do
  n <- get
  put (n + 1)
  pure (TVar (Var n))

-- | Run unification in here.
unifying :: Unifier a -> Infer a
unifying = mapStateT (either (Left . Unify) Right)

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
instantiate :: Scheme -> Infer Ty
instantiate (Forall bound ty) = do
  freshVars <- mapM (const (unifying freshVar)) bound
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
    Nothing -> lift (Left (Unbound name))
    Just sc -> do
      t <- instantiate sc
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
      (context, reduced) = reduceContext mine
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
  case ty of
    TVar _ -> pure (bindings, (evidence, IsIn cls ty) : residual)
    -- A record is an instance when every field is, and an open tail is a
    -- predicate on the tail: Eq {a : Int | r} needs Eq Int and Eq { | r}.
    TRecord row -> case lookupInstance cls "Record" of
      Nothing -> lift (Left (NoInstance (IsIn cls ty)))
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
      Nothing -> lift (Left (NoInstance (IsIn cls ty)))
      Just (key, args) -> case lookupInstance cls key of
        Nothing -> lift (Left (NoInstance (IsIn cls ty)))
        Just inst -> do
          required <- sequence
            [ (\n -> (n, IsIn c arg)) <$> freshEvidence
            | (arg, needs) <- zip args (instRequires inst), c <- needs ]
          (inner, innerResidual) <- simplify required
          let dictionary = foldl EApp (EVar (instanceName cls key)) [EVar n | (n, _) <- required]
          pure (inner <> [(evidence, dictionary)] <> bindings, innerResidual <> residual)

-- | Drop a predicate that another one provides: a duplicate, or a
-- superclass, whose dictionary is a field of its subclass's dictionary.
reduceContext :: [(Name, Pred)] -> ([(Name, Pred)], [(Name, Expr)])
reduceContext preds = go [] [] preds
 where
  go kept bindings [] = (reverse kept, bindings)
  go kept bindings ((evidence, p@(IsIn cls ty)) : rest) =
    case [n | (n, q) <- kept <> rest, q == p, n /= evidence, n `elem` map fst kept] of
      same : _ -> go kept (bindings <> [(evidence, EVar same)]) rest
      [] -> case [ (n, sub) | (n, IsIn sub ty') <- kept <> rest, ty' == ty, cls `elem` superclassesOf sub ] of
        (n, sub) : _ -> go kept (bindings <> [(evidence, superPath sub cls (EVar n))]) rest
        [] -> go ((evidence, p) : kept) bindings rest
  -- The dictionary of a superclass, found through the fields of a subclass's.
  superPath from to dictionary
    | from == to = dictionary
    | otherwise = case [s | ClassInfo name supers <- classes, name == from, s <- supers, s == to || to `elem` superclassesOf s] of
        next : _ -> superPath next to (ESelect dictionary (superField next))
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
  let inType = ftv ty
      keeps (_, p) = mode /= Defaulting && predVars p `Set.isSubsetOf` inType
      (context, reduced) = reduceContext (filter keeps residual)
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
  let vars = Set.toList (Set.unions [predVars p | (_, p) <- preds])
      classesOn v = [cls | (_, IsIn cls (TVar v')) <- preds, v' == v]
      accepts t cls = case tyKey t of
        Just (key, _) -> maybe False (const True) (lookupInstance cls key)
        Nothing -> False
      choose v = case [t | t <- defaultCandidates, all (accepts t) (classesOn v)] of
        t : _ -> t
        [] -> TUnit
      fill = Subst (Map.fromList [(v, choose v) | v <- vars]) (Map.fromList [(v, REmpty) | v <- vars]) Map.empty [] []
      defaulted = [(n, applyPred fill p) | (n, p) <- preds]
  (bindings, residual) <- simplify defaulted `orElse` preds
  case residual of
    [] -> pure (bindings, fill)
    (_, p) : _ -> lift (Left (Ambiguous p))
 where
  -- A candidate that fails gives an ambiguity, not a missing instance.
  orElse action original = do
    state <- get
    case runStateT action state of
      Left (NoInstance _) -> case original of
        (_, p) : _ -> lift (Left (Ambiguous p))
        [] -> pure ([], [])
      Left err -> lift (Left err)
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
commands _ = []

-- | Infer in a starting environment, with the final solution applied. Class
-- predicates on unknowns that the type mentions appear as its context.
inferIn :: Env -> Expr -> Either InferError Ty
inferIn env expr = fst <$> elaborateIn Showing env expr

-- | The type of a line, and the line elaborated with its dictionaries.
elaborateIn :: Mode -> Env -> Expr -> Either InferError (Ty, Expr)
elaborateIn mode env expr = evalStateT go (nextUnknown env)
 where
  go = do
    (s, t, code) <- infer env expr
    (solved, ty) <- finish env s t
    resolveClasses mode solved ty code

checkedConstraints :: Subst -> Infer Subst
checkedConstraints = lift . either (Left . Unify) Right . solveConstraints

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
inferRunIn env expr = (\(_, result, _) -> result) <$> elaborateRunIn env expr

-- | For @x <- e@: the type of the action, the type of what running it gives,
-- and the action elaborated, with its class predicates defaulted.
elaborateRunIn :: Env -> Expr -> Either InferError (Ty, Ty, Expr)
elaborateRunIn env expr = evalStateT go (nextUnknown env)
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

-- Include bound names too: a fresh substitution range must not capture a
-- quantified variable when that substitution reaches another scheme.
nextUnknown :: Env -> Int
nextUnknown env = 1 + maximum (-1 : [number | Forall bound ty <- Map.elems env,
  Var number <- Set.toList (Set.union (Set.fromList bound) (ftv ty))])

-- | Infer with nothing in scope.
inferType :: Expr -> Either InferError Ty
inferType = inferIn Map.empty

