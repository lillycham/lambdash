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
  , Pattern (..)
  , patternVars
  , renderPattern
  , Expr (..)
  , SType (..)
  , SRow (..)
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
  | -- | @let rec f = x -> ... and g = y -> ... in body@. Every bound
    -- expression is a lambda: the parser checks.
    SLetRec [(Name, Surface)] Surface
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
  | -- | @if c then a else b@.
    SIf Surface Surface Surface
  | -- | @case e of p -> a; q -> b end@.
    SCase Surface [(Pattern, Surface)]
  deriving (Eq, Show)

-- | A pattern in a @case@ arm.
data Pattern
  = -- | @_@: anything, bound to nothing.
    PWild
  | -- | @x@: anything, bound to @x@.
    PVar Name
  | -- | @Just x@: a constructor of a declared type, and a pattern for each
    -- of its fields.
    PCon Name [Pattern]
  | -- | @0@, @"s"@, @true@, @()@: that value only.
    PLit Lit
  deriving (Eq, Show)

-- | A pattern as written. A constructor with fields is bracketed where it is
-- itself a field.
renderPattern :: Pattern -> String
renderPattern = go False
 where
  go nested p = case p of
    PWild -> "_"
    PVar name -> name
    PCon name [] -> name
    PCon name fields -> (if nested then \s -> "(" <> s <> ")" else id) (unwords (name : map (go True) fields))
    PLit (LInt n) -> (if nested && n < 0 then \s -> "(" <> s <> ")" else id) (show n)
    PLit (LBool b) -> if b then "true" else "false"
    PLit LUnit -> "()"
    PLit (LStr s) -> show s
    PLit (LRef s) -> s

-- | The names a pattern binds, left to right.
patternVars :: Pattern -> [Name]
patternVars p = case p of
  PVar name -> [name]
  PCon _ fields -> concatMap patternVars fields
  _ -> []

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
  | -- | @Comp a@, @Job a@, @Stream i o r@, @Stage i o@, @Collector i o@,
    -- and a row if one is written: @Comp {console | e} a@.
    STApp Name (Maybe SRow) [SType]
  deriving (Eq, Show)

-- | An effect row as written: its labels, such as @exec git@ and
-- @console@, and its tail if it is open.
data SRow = SRow [String] (Maybe Name)
  deriving (Eq, Show)

-- | @Eq a@ in a context.
data SPred = SPred Name SType
  deriving (Eq, Show)

-- | A line that declares a class, an instance or a data type.
data Declaration
  = -- | @class (Eq a) => Hashable a where hash : a -> Int@: the superclasses,
    -- the class, its unknown, its methods' types, and default definitions.
    DeclClass [SPred] Name Name [(Name, SType)] [(Name, Surface)]
  | -- | @instance Show a => Show [a] where show = ...@: the context, the
    -- class, the head, and the methods.
    DeclInstance [SPred] Name SType [(Name, Surface)]
  | -- | @data Maybe a = Nothing | Just a@: the type, its parameters, and
    -- each constructor with the types of its fields.
    DeclData Name [Name] [(Name, [SType])]
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
  | -- | @let rec f = ... and g = ... in body@: every name is in scope in
    -- every bound expression, and each of those is a lambda. Inside the
    -- group a name has one type; the group generalises after.
    ELetRec [(Name, Expr)] Expr
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
  | -- | @if c then a else b@. Two computations join their rows: the line
    -- may do either.
    EIf Expr Expr Expr
  | -- | @case e of p -> a; q -> b end@: the first arm whose pattern
    -- matches. The checker makes sure that some arm always does.
    ECase Expr [(Pattern, Expr)]
  deriving (Eq, Show)
