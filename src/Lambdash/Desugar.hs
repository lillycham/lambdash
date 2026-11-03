-- | Surface syntax to core. Everything here is sugar: what is left for the
-- checker has its own type rule.
--
-- > x y -> e                    \x -> \y -> e
-- > .a.b                        \r -> r.a.b
-- > do x <- a; y = b; c end      bind x a (let y = b in run c)
-- > do a; c end                  bind %1 a (run c)
-- > use REF                      bind %1 (%pkg REF) (run (%use %1))
-- > with REF do ... end          bind %1 (%pkg REF) (run (with %1 do ... end))
--
-- Names that desugaring invents start with @%@, which no identifier can
-- contain, so they capture nothing. The sugar refers to the builtins through
-- aliases of the same kind, @%pkg@ and @%use@, so it means the same whatever
-- the session binds.
module Lambdash.Desugar
  ( desugar
  , stripLocations
  , builtinAlias
  , renderCore
  ) where

import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import Data.List (intercalate)
import Lambdash.Infer (Expr (..))
import Lambdash.Syntax

-- | The name by which sugar refers to a builtin.
builtinAlias :: Name -> Name
builtinAlias name = '%' : name

-- | Desugar one expression. Every node keeps its span.
desugar :: Surface -> Expr
desugar surface = evalState (go surface) 1

fresh :: State Int Name
fresh = do
  n <- get
  put (n + 1)
  pure ('%' : show n)

go :: Surface -> State Int Expr
go (Surface location node) = ELoc location <$> case node of
  SVar name -> pure (EVar name)
  SLit lit -> pure (ELit lit)
  SLam names body -> (\b -> foldr ELam b names) <$> go body
  SApp f x -> EApp <$> go f <*> go x
  SPipe a b -> EPipe <$> go a <*> go b
  SLet name bound body -> ELet name <$> go bound <*> go body
  SCmd name args -> pure (ECmd name args)
  SRecord fields -> ERecord <$> traverse (\(label, value) -> (,) label <$> go value) fields
  SList items -> EList <$> traverse go items
  SSelect record label -> (`ESelect` label) <$> go record
  -- The body mentions only r, so r cannot capture anything.
  SAccessor labels -> pure (ELam "r" (foldl ESelect (EVar "r") labels))
  SDo statements -> block statements
  SWith (Surface _ (SLit (LRef ref))) body -> do
    overlay <- fresh
    inner <- go body
    pure (EBind overlay (resolve ref) (ERun (EWith (EVar overlay) inner)))
  SWith overlay body -> EWith <$> go overlay <*> go body
  SBg e -> EBg <$> go e
  SUseRef headName ref -> do
    overlay <- fresh
    pure (EBind overlay (resolve ref) (ERun (EApp (EVar (builtinAlias headName)) (EVar overlay))))
 where
  resolve ref = EApp (EVar (builtinAlias "pkg")) (ELit (LStr ref))

-- | A block is a chain of binds that ends in a run. The parser guarantees a
-- last statement that is an expression.
block :: [Statement] -> State Int Expr
block statements = case statements of
  [StmtRun e] -> ERun <$> go e
  StmtRun e : rest -> do
    name <- fresh
    EBind name <$> go e <*> block rest
  StmtBind name e : rest -> EBind name <$> go e <*> block rest
  StmtLet name e : rest -> ELet name <$> go e <*> block rest
  _ -> error "desugar: the parser passed a block that does not end with an expression"

-- | The same expression without its spans, for comparing shapes.
stripLocations :: Expr -> Expr
stripLocations expression = case expression of
  ELoc _ e -> stripLocations e
  ELam name body -> ELam name (stripLocations body)
  EApp f x -> EApp (stripLocations f) (stripLocations x)
  EPipe a b -> EPipe (stripLocations a) (stripLocations b)
  ELet name bound body -> ELet name (stripLocations bound) (stripLocations body)
  ERecord fields -> ERecord [(label, stripLocations value) | (label, value) <- fields]
  EList items -> EList (map stripLocations items)
  ESelect record label -> ESelect (stripLocations record) label
  EBind name e body -> EBind name (stripLocations e) (stripLocations body)
  ERun e -> ERun (stripLocations e)
  EWith overlay body -> EWith (stripLocations overlay) (stripLocations body)
  EBg e -> EBg (stripLocations e)
  other -> other

-- | Core as text, for @:core@. It reads like the surface syntax where core
-- has the same form. The forms that only core has get words of their own:
--
-- > bind x <- e; rest           run e, name its result, continue
-- > run e                       run a stream or a computation
-- > bg e                        start e as a job
-- > with o e                    run e with one more layer
renderCore :: Expr -> String
renderCore = render 0
 where
  -- 0: anything. 1: a pipeline stage. 2: a function. 3: an argument.
  render :: Int -> Expr -> String
  render level expression = case expression of
    ELoc _ e -> render level e
    EVar name -> name
    ELit lit -> literal lit
    ELam _ _ ->
      let (names, body) = lambdas expression
      in parens (level > 0) (unwords names <> " -> " <> render 0 body)
    EApp f x -> parens (level > 2) (render 2 f <> " " <> render 3 x)
    EPipe a b -> parens (level > 1) (render 1 a <> " | " <> render 2 b)
    ELet name bound body -> parens (level > 0) ("let " <> name <> " = " <> render 0 bound <> " in " <> render 0 body)
    ECmd name args -> parens (level > 2) (unwords (name : map argument args))
    ERecord fields -> "{" <> intercalate ", " [label <> " = " <> render 0 value | (label, value) <- fields] <> "}"
    EList items -> "[" <> intercalate ", " (map (render 0) items) <> "]"
    ESelect record label -> render 3 record <> "." <> label
    EBind name e body -> parens (level > 0) ("bind " <> name <> " <- " <> render 0 e <> "; " <> render 0 body)
    ERun e -> parens (level > 2) ("run " <> render 3 e)
    EWith overlay body -> parens (level > 2) ("with " <> render 3 overlay <> " " <> render 3 body)
    EBg e -> parens (level > 2) ("bg " <> render 3 e)
  lambdas (ELoc _ e) = lambdas e
  lambdas (ELam name body) = let (names, inner) = lambdas body in (name : names, inner)
  lambdas e = ([], e)
  parens True text = "(" <> text <> ")"
  parens False text = text
  literal (LInt n) = show n
  literal (LBool b) = if b then "true" else "false"
  literal LUnit = "()"
  literal (LStr text) = show text
  literal (LRef ref) = ref
  argument (AWord word) = if any (`elem` (" \"$|;&()[]{}" :: String)) word || null word then show word else word
  argument (AVar name) = '$' : name
