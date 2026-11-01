-- | The expression parser. Syntax is provisional.
--
-- > x y -> body                 lambda, one or more parameters
-- > let x = 1 in x              let
-- > f x y                       application, to the left
-- > {name = 1, ok = true}       record
-- > r.name                      select, with no space before the dot
-- > .name  .a.b                 accessor: \r -> r.name
-- > 1  true  false  x  (e)      atoms
-- > -- to the end of the line   comment
module Lambdash.Parser
  ( Parser
  , sc
  , lexeme
  , symbol
  , keyword
  , identifier
  , literal
  , record
  , accessor
  , atom
  , postfix
  , application
  , lambda
  , letIn
  , expr
  , parseExpr
  ) where

import Data.Bifunctor (first)
import Data.Void (Void)
import Lambdash.Infer (Expr (..), Lit (..), Name)
import Lambdash.Unify (Label)
import Text.Megaparsec
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String

-- | Skip spaces and comments.
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") empty

-- | Run a parser, then skip what follows it.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | A fixed token, then skip what follows it.
symbol :: String -> Parser String
symbol = L.symbol sc

-- | A reserved word. @let@ matches in @let x@, but not in @letter@.
keyword :: String -> Parser ()
keyword = undefined

-- | A letter, then letters, digits, @_@ or @'@. It refuses the reserved words:
-- @let@, @in@, @true@ and @false@.
--
-- Note: 'atom' and the parsers it uses leave trailing space alone. See 'postfix'.
identifier :: Parser Name
identifier = undefined

-- | An integer, @true@ or @false@.
literal :: Parser Lit
literal = undefined

-- | @{a = 1, b = 2}@, and @{}@. A later field with the same name is an ordinary
-- field: the checker decides what that means.
record :: Parser Expr
record = undefined

-- | @.a.b@ means @\\r -> r.a.b@. Use the name @r@, which cannot capture anything,
-- because the body mentions nothing else.
accessor :: Parser Expr
accessor = undefined

-- | One thing that needs no parentheses. It does not skip trailing space.
atom :: Parser Expr
atom = undefined

-- | An atom, then any number of @.label@, then skip what follows.
--
-- This is why 'atom' leaves trailing space alone: @r.a@ selects, but in @f .a@
-- the space means that @.a@ is an argument. If 'atom' ate the space, the two
-- would look the same here.
postfix :: Parser Expr
postfix = undefined

-- | One or more 'postfix', applied to the left: @f x y@ is @(f x) y@.
application :: Parser Expr
application = undefined

-- | @x y -> body@ means @\\x -> \\y -> body@. The body goes as far right as it
-- can.
--
-- Note: @f x@ starts like a lambda until the arrow fails to arrive. Look at 'try'.
lambda :: Parser Expr
lambda = undefined

-- | @let x = bound in body@.
letIn :: Parser Expr
letIn = undefined

-- | Any expression.
expr :: Parser Expr
expr = undefined

-- | Parse one whole expression, or give a readable error.
parseExpr :: String -> Either String Expr
parseExpr = first errorBundlePretty . parse (sc *> expr <* eof) "<input>"
