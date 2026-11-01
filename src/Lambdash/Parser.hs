-- | The expression parser. Syntax is provisional.
--
-- > x y -> body                 lambda, one or more parameters
-- > let x = 1 in x              let
-- > f x y                       application, to the left
-- > {name = 1, ok = true}       record
-- > r.name                      select, with no space before the dot
-- > .name  .a.b                 accessor: \r -> r.name
-- > 1  true  false  ()  x  (e)  atoms
-- > -- to the end of the line   comment
module Lambdash.Parser
  ( Parser,
    sc,
    lexeme,
    symbol,
    keyword,
    identifier,
    literal,
    record,
    accessor,
    atom,
    postfix,
    application,
    lambda,
    letIn,
    expr,
    parseExpr,
  )
where

import Data.Bifunctor (first)
import Data.Void (Void)
import Lambdash.Infer (Expr (..), Lit (..), Name)
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, space1, string)
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

reservedNames :: [String]
reservedNames =
  [ "let",
    "in",
    "true",
    "false"
  ]

-- | A reserved word. @let@ matches in @let x@, but not in @letter@.
keyword :: String -> Parser ()
keyword s = try $ string s *> notFollowedBy identifierChar

identifierChar :: Parser Char
identifierChar = alphaNumChar <|> char '_' <|> char '\''

-- | A letter, then letters, digits, @_@ or @'@. It refuses the reserved words:
-- @let@, @in@, @true@ and @false@.
--
-- Note: 'atom' and the parsers it uses leave trailing space alone. See 'postfix'.
identifier :: Parser Name
identifier = try (name >>= check)
  where
    name = (:) <$> letterChar <*> many identifierChar
    check x
      | x `elem` reservedNames = fail $ show x <> " cannot be an identifier"
      | otherwise = pure x

integer :: Parser Int
integer = L.decimal

signedInteger :: Parser Int
signedInteger = L.signed (pure ()) integer

-- | An integer, @true@ or @false@.
literal :: Parser Lit
literal =
  LInt <$> signedInteger
    <|> LBool True <$ keyword "true"
    <|> LBool False <$ keyword "false"

-- | @{a = 1, b = 2}@, and @{}@. A later field with the same name is an ordinary
-- field: the checker decides what that means.
record :: Parser Expr
record = ERecord <$> between (symbol "{") (char '}') (field `sepBy` symbol ",")
  where
    field = (,) <$> lexeme identifier <*> (symbol "=" *> expr)

-- | @.a.b@ means @\\r -> r.a.b@. Use the name @r@, which cannot capture anything,
-- because the body mentions nothing else.
accessor :: Parser Expr
accessor = do
  labels <- some (char '.' *> identifier)
  let body = foldl ESelect (EVar "r") labels
  pure (ELam "r" body)

-- | One thing that needs no parentheses. It does not skip trailing space.
atom :: Parser Expr
atom =
  (ELit <$> literal)
    <|> record
    <|> accessor
    <|> (EVar <$> identifier)
    <|> (ELit LUnit <$ try (symbol "(" *> char ')'))
    <|> between (symbol "(") (char ')') expr

-- | An atom, then any number of @.label@, then skip what follows.
--
-- This is why 'atom' leaves trailing space alone: @r.a@ selects, but in @f .a@
-- the space means that @.a@ is an argument. If 'atom' ate the space, the two
-- would look the same here.
postfix :: Parser Expr
postfix = do
  e <- atom
  labels <- many (char '.' *> identifier)
  sc
  pure (foldl ESelect e labels)

-- | One or more 'postfix', applied to the left: @f x y@ is @(f x) y@.
application :: Parser Expr
application = do
  fun <- postfix
  args <- many postfix
  pure (foldl EApp fun args)

-- | @x y -> body@ means @\\x -> \\y -> body@. The body goes as far right as it
-- can.
--
-- Note: @f x@ starts like a lambda until the arrow fails to arrive. Look at 'try'.
lambda :: Parser Expr
lambda = do
  names <- try (some (lexeme identifier) <* symbol "->")
  body <- expr
  pure (foldr ELam body names)

-- | @let x = bound in body@.
letIn :: Parser Expr
letIn = do
  lexeme (keyword "let")
  name <- lexeme identifier
  _ <- symbol "="
  bound <- expr
  lexeme (keyword "in")
  body <- expr
  pure (ELet name bound body)

-- | Any expression.
expr :: Parser Expr
expr = letIn <|> lambda <|> application

-- | Parse one whole expression, or give a readable error.
parseExpr :: String -> Either String Expr
parseExpr = first errorBundlePretty . parse (sc *> expr <* eof) "<input>"
