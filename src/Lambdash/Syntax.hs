-- | The surface syntax: what the parser produces, before desugaring.
--
-- Every node carries its source span. 'Lambdash.Desugar' turns this into the
-- core that the checker and the evaluator see, and keeps the spans there, so
-- that later errors can point at the code.
module Lambdash.Syntax
  ( Name
  , Lit (..)
  , Arg (..)
  , Span (..)
  , Surface (..)
  , Node (..)
  , Statement (..)
  , Expr (..)
  , SType (..)
  , SPred (..)
  , Declaration (..)
  ) where

import Lambdash.Unify (Label)

type Name = String

data Lit
  = LInt Int
  | LBool Bool
  | LUnit
  | LStr String
  | -- | A bare flake reference, such as @nixpkgs#ffmpeg@. It is a String.
    LRef String
  deriving (Eq, Show)

-- | One argument of an external command.
data Arg
  = -- | A bareword or a string literal.
    AWord String
  | -- | @$x@.
    AVar Name
  deriving (Eq, Show)

-- | Character offsets into the line, start inclusive, end exclusive.
--
-- The end can include whitespace that a token consumed after itself, and a
-- parenthesised expression starts inside its parentheses. Trim the text when
-- showing a span.
--
-- Spans never decide equality, so two trees that differ only in where they
-- came from are equal. Tests compare shapes, not positions.
data Span = Span Int Int

instance Eq Span where
  _ == _ = True

instance Show Span where
  show (Span start end) = "Span " <> show start <> " " <> show end

data Surface = Surface Span Node
  deriving (Eq, Show)

data Node
  = SVar Name
  | SLit Lit
  | -- | @x y -> body@.
    SLam [Name] Surface
  | SApp Surface Surface
  | SPipe Surface Surface
  | SLet Name Surface Surface
  | SCmd Name [Arg]
  | SRecord [(Label, Surface)]
  | SList [Surface]
  | SSelect Surface Label
  | -- | @.a.b@.
    SAccessor [Label]
  | -- | @do s; s; e end@.
    SDo [Statement]
  | -- | @with overlay do ... end@. A bare reference resolves with @pkg@.
    SWith Surface Surface
  | -- | @e &@.
    SBg Surface
  | -- | @use REF@ or @useLocal REF@, while the head is still the builtin.
    SUseRef Name String
  | -- | @a op b@, after precedence.
    SOp Name Surface Surface
  | -- | @(op e)@, a right section: @x -> x op e@.
    SRightSection Name Surface
  | -- | @(e op)@, a left section: @x -> e op x@.
    SLeftSection Surface Name
  | -- | @(.size > 1)@: an operator expression whose leftmost operand is an
    -- accessor is a function of the record.
    SAccessorSection Surface
  | -- | @(e : t)@.
    SAnn Surface SType
  deriving (Eq, Show)

data Statement
  = -- | @x <- e@.
    StmtBind Name Surface
  | -- | @x = e@.
    StmtLet Name Surface
  | -- | @e@.
    StmtRun Surface
  deriving (Eq, Show)

-- | A type as written. A lowercase name is an unknown, and each @Comp@,
-- @Job@ and @Stream@ has an effect row of its own, unknown.
data SType
  = STVar Name
  | STCon Name
  | STList SType
  | STFun SType SType
  | -- | @{a : Int | r}@: the fields, and the tail if there is one.
    STRecord [(Label, SType)] (Maybe Name)
  | -- | @Comp a@, @Job a@, @Stream i o r@, @Stage i o@, @Collector i o@.
    STApp Name [SType]
  deriving (Eq, Show)

-- | @Eq a@ in a context.
data SPred = SPred Name SType
  deriving (Eq, Show)

-- | A line that declares a class or an instance.
data Declaration
  = -- | @class (Eq a) => Hashable a where hash : a -> Int@: the superclasses,
    -- the class, its unknown, its methods' types, and default definitions.
    DeclClass [SPred] Name Name [(Name, SType)] [(Name, Surface)]
  | -- | @instance Show a => Show [a] where show = ...@: the context, the
    -- class, the head, and the methods.
    DeclInstance [SPred] Name SType [(Name, Surface)]
  deriving (Eq, Show)

-- | Core: what desugaring leaves, for the checker and the evaluator.
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
  | -- | @(e : t)@: @e@, at a type no more general than @t@.
    EAnn Expr SType
  deriving (Eq, Show)
