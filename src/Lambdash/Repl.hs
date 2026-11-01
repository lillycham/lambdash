-- | The REPL as a pure function: a session and a line give a new session and
-- some text. The loop in @app/Main.hs@ only moves text, so everything here can
-- be tested without a terminal.
module Lambdash.Repl
  ( Session (..)
  , initial
  , Command (..)
  , command
  , parseCommand
  , renderType
  , renderError
  , Step (..)
  , step
  ) where

import Data.Bifunctor (first)
import Lambdash.Eval
import Lambdash.Infer
import Lambdash.Parser
import Lambdash.Unify
import Text.Megaparsec hiding (State)

-- | What persists between lines. A name has its type in one and its value in
-- the other, so an update has to change both.
data Session = Session
  { sessionTypes :: Env
  , sessionValues :: Scope
  }

-- | The session before any input.
initial :: Session
initial = Session typeEnv scope

-- | What one line asks for.
data Command
  = -- | Nothing, or only a comment.
    Blank
  | -- | @:q@.
    Exit
  | -- | @:t expr@.
    TypeOf Expr
  | -- | @x = expr@.
    Bind Name Expr
  | -- | @expr@.
    Run Expr
  deriving (Eq, Show)

-- | One line.
--
-- Note: @x = 1@ starts like the expression @x@ until the @=@ arrives. Reserved
-- words help too: @keyword ":t"@ works here.
command :: Parser Command
command = undefined

-- | Parse one whole line, or give a readable error.
parseCommand :: String -> Either String Command
parseCommand = first errorBundlePretty . parse (sc *> command <* eof) "<input>"

-- | Show a type. Unknowns get letters in first-seen order, so the numbers from
-- the checker never reach the user: @a -> a@, @(a -> b) -> a -> b@,
-- @{size : a | b} -> a@, @{a : Int, b : Bool}@, @{}@.
--
-- Note: an unknown can be in a row tail as well as in a type, and they share
-- letters. Only the left of an arrow needs parentheses.
renderType :: Ty -> String
renderType = undefined

-- | Say what went wrong, in words for a person who typed a line.
--
-- > unbound name: x
-- > type mismatch: Int and Bool
-- > infinite type: a and a -> b
-- > no field: nmae
renderError :: InferError -> String
renderError = undefined

-- | The result of one line.
data Step
  = -- | The next session, and text to show. No text means show nothing.
    Continue Session String
  | Quit

-- | Run one line. A line that fails leaves the session as it was.
--
-- Note: think about what a stored type needs, so that @i = x -> x@ still works
-- at two types on later lines.
step :: Session -> String -> Step
step = undefined
