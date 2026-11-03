-- | The expression parser. Syntax is provisional.
--
-- > x y -> body                 lambda, one or more parameters
-- > let x = 1 in x              let
-- > f x y                       application, to the left
-- > {name = 1, ok = true}       record
-- > r.name                      select, with no space before the dot
-- > .name  .a.b                 accessor: \r -> r.name
-- > 1  "a"  true  false  ()  x  (e)  atoms
-- > git log --oneline $x        external command, when nothing binds the head
-- > source | stage | stage      pipeline, below application precedence
-- > do x <- e; f x end           a block of statements, run in order
-- > with o do e end              a block with one more environment layer
-- > -- to the end of the line   comment
module Lambdash.Parser
  ( Parser,
    Bound (..),
    runIn,
    parseExprIn,
    binding,
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
    doBlock,
    expr,
    parseExpr,
  )
where

import Control.Monad (when)
import Control.Monad.Reader (Reader, asks, local, runReader)
import Data.Bifunctor (first)
import Data.Char (isSpace)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Void (Void)
import Lambdash.Infer (Arg (..), Expr (..), Lit (..), Name, Stmt (..))
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, hspace, letterChar, space1, string)
import Text.Megaparsec.Char.Lexer qualified as L

-- | The names that an unbound head is told apart from. Parsing never reads
-- `PATH`, so nothing outside the session can change how a line parses.
data Bound
  = -- | Every name is bound, so nothing is an external command.
    AllBound
  | BoundNames (Set Name)

isBound :: Name -> Bound -> Bool
isBound _ AllBound = True
isBound name (BoundNames names) = Set.member name names

-- | Add names for the length of a parser.
binding :: [Name] -> Parser a -> Parser a
binding names = local (\context -> context {contextBound = add (contextBound context)})
 where
  add AllBound = AllBound
  add (BoundNames bound) = BoundNames (Set.union (Set.fromList names) bound)

data ParseContext = ParseContext
  { contextBound :: Bound
  , commaDelimiter :: Bool
  , -- | Inside @do ... end@, the word @end@ ends a command line.
    inBlock :: Bool
  }

type Parser = ParsecT Void String (Reader ParseContext)

withComma :: Bool -> Parser a -> Parser a
withComma active = local (\context -> context {commaDelimiter = active})

-- | Run a parser, given what is bound.
runIn :: Bound -> Parser a -> String -> Either (ParseErrorBundle String Void) a
runIn bound p input = runReader (runParserT p "<input>" input) (ParseContext bound False False)

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
    "false",
    "do",
    "end",
    "with"
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
  LStr <$> stringLiteral
    <|> LInt <$> signedInteger
    <|> LBool True <$ keyword "true"
    <|> LBool False <$ keyword "false"

-- | @{a = 1, b = 2}@, and @{}@. A later field with the same name is an ordinary
-- field: the checker decides what that means.
record :: Parser Expr
record = ERecord <$> between (symbol "{") (char '}') (field `sepBy` symbol ",")
  where
    field = (,) <$> lexeme identifier <*> (symbol "=" *> withComma True expr)

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
  doBlock
    <|> withBlock
    <|> (ELit <$> literal)
    <|> record
    <|> (EList <$> between (symbol "[") (char ']') (withComma True expr `sepBy` symbol ","))
    <|> accessor
    <|> (EVar <$> identifier)
    <|> (ELit LUnit <$ try (symbol "(" *> char ')'))
    <|> between (symbol "(") (char ')') (withComma False expr)

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
-- | A string in double quotes, with the escapes of Haskell.
stringLiteral :: Parser String
stringLiteral = char '"' *> manyTill L.charLiteral (char '"')

-- | One character of a bareword. These end a word because the line needs them.
wordChar :: Parser Char
wordChar = do
  comma <- asks commaDelimiter
  satisfy (\c -> not (isSpace c) && c `notElem` (")}]|\"$;" :: String) && not (comma && c == ','))

-- | The head of an external command: a path, or a word that starts with a name
-- that nothing binds. A bound name is left for 'application'.
commandHead :: Parser Name
commandHead = do
  isPath <- option False (True <$ lookAhead (choice (map string ["/", "./", "../", "~/"])))
  if isPath
    then do
      -- The empty name is bound only under 'AllBound'.
      everything <- asks (isBound "" . contextBound)
      when everything (fail "no external commands here")
    else do
      name <- lookAhead identifier
      bound <- asks (isBound name . contextBound)
      when bound (fail "a bound name is not a command")
  some wordChar

-- | One argument of an external command.
argument :: Parser Arg
argument = do
  block <- asks inBlock
  when block (notFollowedBy (keyword "end"))
  AVar <$> (char '$' *> identifier)
    <|> AWord <$> stringLiteral
    <|> AWord <$> some wordChar

-- | @git log --oneline $x@. Words end a line or a bracket, and @--@ is an
-- ordinary word here, not a comment.
external :: Parser Expr
external = do
  name <- try commandHead
  hspace
  args <- many (argument <* hspace)
  sc
  pure (ECmd name args)

application :: Parser Expr
application = external <|> do
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
  body <- binding names expr
  pure (foldr ELam body names)

-- | @let x = bound in body@.
letIn :: Parser Expr
letIn = do
  lexeme (keyword "let")
  name <- lexeme identifier
  _ <- symbol "="
  bound <- expr
  lexeme (keyword "in")
  body <- binding [name] expr
  pure (ELet name bound body)

-- | @do s; s; e end@. A statement is @x <- e@, @x = e@ or @e@, and the last
-- one is an expression. A name that a statement binds is bound for the
-- statements after it, so it is a function there, not a command. A @;@ may
-- come before @end@.
--
-- Note: inside a block, a bare @end@ ends a command line. Quote it to pass it.
doBlock :: Parser Expr
doBlock = do
  lexeme (keyword "do")
  stmts <- local (\context -> context {inBlock = True, commaDelimiter = False}) statements
  keyword "end"
  case reverse stmts of
    SRun _ : _ -> pure (EDo stmts)
    _ -> fail "a block must end with an expression"
 where
  statements = do
    stmt <- statement
    let names = case stmt of
          SBind name _ -> [name]
          SLet name _ -> [name]
          SRun _ -> []
    more <- option [] $ do
      _ <- symbol ";"
      option [] (notFollowedBy (keyword "end") *> binding names statements)
    pure (stmt : more)
  statement =
        (SBind <$> try (lexeme identifier <* symbol "<-") <*> expr)
    <|> (SLet <$> try (lexeme identifier <* symbol "=" <* notFollowedBy (char '=')) <*> expr)
    <|> (SRun <$> expr)

-- | @with overlay do ... end@.
withBlock :: Parser Expr
withBlock = do
  lexeme (keyword "with")
  overlay <- postfix
  EWith overlay <$> doBlock

-- | Any expression.
expr :: Parser Expr
expr = letIn <|> lambda <|> pipeline
 where
  pipeline = do
    firstStage <- application
    rest <- many (symbol "|" *> (lambda <|> application))
    pure (foldl EPipe firstStage rest)

-- | Parse one whole expression, or give a readable error.
parseExpr :: String -> Either String Expr
parseExpr = parseExprIn AllBound

-- | The same, given what is bound.
parseExprIn :: Bound -> String -> Either String Expr
parseExprIn bound = first errorBundlePretty . runIn bound (sc *> expr <* eof)
