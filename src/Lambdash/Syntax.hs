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
  deriving (Eq, Show)

data Statement
  = -- | @x <- e@.
    StmtBind Name Surface
  | -- | @x = e@.
    StmtLet Name Surface
  | -- | @e@.
    StmtRun Surface
  deriving (Eq, Show)
