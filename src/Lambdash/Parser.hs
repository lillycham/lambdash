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
-- > sleep 10 &                   a background job; gives its handle
-- > nixpkgs#ffmpeg  .#dev        a flake reference: a String
-- > use nixpkgs#ffmpeg           sugar for do o <- pkg "nixpkgs#ffmpeg"; use o end
-- > with .#bench do e end        sugar for do o <- pkg ".#bench"; with o do e end end
--
-- The parser builds surface syntax with spans. "Lambdash.Desugar" turns it
-- into core.
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
    flakeRef,
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
    parseSurfaceIn,
  )
where

import Control.Monad (when)
import Control.Monad.Reader (Reader, asks, local, runReader)
import Data.Bifunctor (first)
import Data.Char (isAlphaNum, isSpace)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Void (Void)
import Lambdash.Desugar (desugar, stripLocations)
import Lambdash.Infer (Expr)
import Lambdash.Syntax
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, hspace, letterChar, space1, string)
import Text.Megaparsec.Char.Lexer qualified as L

-- | The names that an unbound head is told apart from. Parsing never reads
-- `PATH`, so nothing outside the session can change how a line parses.
data Bound
  = -- | Every name is bound, so nothing is an external command.
    AllBound
  | BoundNames (Set Name)
  | -- | Bound names, and the builtins that still have their builtin value,
    -- whose signatures guide parsing. A rebound @ls@ is an ordinary function.
    BoundWith (Set Name) (Set Name)

isBound :: Name -> Bound -> Bool
isBound _ AllBound = True
isBound name (BoundWith names _) = Set.member name names
isBound name (BoundNames names) = Set.member name names

-- | Add names for the length of a parser.
binding :: [Name] -> Parser a -> Parser a
binding names = local (\context -> context {contextBound = add (contextBound context)})
 where
  add AllBound = AllBound
  add (BoundNames bound) = BoundNames (Set.union (Set.fromList names) bound)
  -- A local name hides a builtin's signature.
  add (BoundWith bound builtin) = BoundWith (Set.union (Set.fromList names) bound) (Set.difference builtin (Set.fromList names))

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

-- | Give a node the span of the text it was parsed from.
spanned :: Parser Node -> Parser Surface
spanned p = do
  start <- getOffset
  node <- p
  end <- getOffset
  pure (Surface (Span start end) node)

-- | A node built from an earlier one: from its start to here.
extend :: Surface -> Node -> Parser Surface
extend (Surface (Span start _) _) node = do
  end <- getOffset
  pure (Surface (Span start end) node)

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
record :: Parser Surface
record = spanned (SRecord <$> between (symbol "{") (char '}') (field `sepBy` symbol ","))
  where
    field = (,) <$> lexeme identifier <*> (symbol "=" *> withComma True expr)

-- | @.a.b@ means @\\r -> r.a.b@.
accessor :: Parser Surface
accessor = spanned (SAccessor <$> some (char '.' *> identifier))

-- | A bare flake reference: a word with a @#@, such as @nixpkgs#ffmpeg@, @.#@,
-- @./path#dev@ or @github:owner/repo#x@. Nothing else in an expression uses
-- @#@, so a word with one is a reference.
flakeRef :: Parser String
flakeRef = try $ do
  word <- some (satisfy refChar)
  if '#' `elem` word then pure word else fail "not a flake reference"
 where
  refChar c = isAlphaNum c || c `elem` ("._-/:~+@?=&#%" :: String)

-- | One thing that needs no parentheses. It does not skip trailing space.
atom :: Parser Surface
atom =
  doBlock
    <|> withBlock
    <|> spanned (SLit . LRef <$> flakeRef)
    <|> spanned (SLit <$> literal)
    <|> record
    <|> spanned (SList <$> between (symbol "[") (char ']') (withComma True expr `sepBy` symbol ","))
    <|> accessor
    <|> spanned (SVar <$> identifier)
    <|> spanned (SLit LUnit <$ try (symbol "(" *> char ')'))
    <|> between (symbol "(") (char ')') (withComma False expr)

-- | An atom, then any number of @.label@, then skip what follows.
--
-- This is why 'atom' leaves trailing space alone: @r.a@ selects, but in @f .a@
-- the space means that @.a@ is an argument. If 'atom' ate the space, the two
-- would look the same here.
postfix :: Parser Surface
postfix = do
  e <- atom
  selected <- selects e
  sc
  pure selected
 where
  selects e = option e $ do
    field <- char '.' *> identifier
    extend e (SSelect e field) >>= selects

-- | One or more 'postfix', applied to the left: @f x y@ is @(f x) y@.
-- | A string in double quotes, with the escapes of Haskell.
stringLiteral :: Parser String
stringLiteral = char '"' *> manyTill L.charLiteral (char '"')

-- | One character of a bareword. These end a word because the line needs them.
wordChar :: Parser Char
wordChar = do
  comma <- asks commaDelimiter
  satisfy (\c -> not (isSpace c) && c `notElem` (")}]|\"$;&" :: String) && not (comma && c == ','))

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
external :: Parser Surface
external = do
  start <- getOffset
  name <- try commandHead
  hspace
  args <- many (argument <* hspace)
  end <- getOffset
  sc
  pure (Surface (Span start end) (SCmd name args))

application :: Parser Surface
application = external <|> do
  fun <- postfix
  slotted <- case fun of
    Surface _ (SVar name) -> do
      signature <- asks (signatureOf name . contextBound)
      maybe (pure []) slotArgs signature
    _ -> pure []
  args <- many postfix
  builtin <- asks (isBuiltin . contextBound)
  case (fun, slotted <> args) of
    -- A bare reference means pkg, as in the plan, while the head is still
    -- the builtin. The desugared form refers to the builtins themselves.
    (Surface _ (SVar name), [Surface _ (SLit (LRef ref))])
      | name `elem` ["use", "useLocal"], builtin name -> extend fun (SUseRef name ref)
    (_, all') -> foldApply fun all'
 where
  foldApply f [] = pure f
  foldApply f (x : xs) = extend f (SApp f x) >>= (`foldApply` xs)

-- | Whether a name still means its builtin, as far as parsing knows. Without
-- the session's builtins, as in tests, it does.
isBuiltin :: Bound -> Name -> Bool
isBuiltin (BoundWith _ builtin) name = Set.member name builtin
isBuiltin _ _ = True

-- | One argument position of a builtin, for parsing.
data Slot
  = -- | A bareword here is a String, as in an external command. With a
    -- default, the argument may be left out.
    PathSlot (Maybe String)

-- | The builtins whose arguments are paths, so that @ls src@ and @cd ..@ need
-- no quotes. Other builtins take expressions: @filter .ok@ still works. This
-- is Nushell's signature-directed parsing.
signatures :: [(Name, [Slot])]
signatures =
  [ ("cd", [PathSlot (Just "~")])
  , ("ls", [PathSlot (Just ".")])
  ]

signatureOf :: Name -> Bound -> Maybe [Slot]
signatureOf name (BoundWith _ builtin)
  | Set.member name builtin = lookup name signatures
signatureOf _ _ = Nothing

-- | Arguments for path slots. A missing argument with a default ends the
-- slots: @ls | filter f@ is @ls "." | filter f@.
slotArgs :: [Slot] -> Parser [Surface]
slotArgs [] = pure []
slotArgs (PathSlot fallback : rest) = do
  given <- optional pathArg
  case given of
    Just arg -> (arg :) <$> slotArgs rest
    Nothing -> do
      here <- getOffset
      pure (maybe [] (\path -> [Surface (Span here here) (SLit (LStr path))]) fallback)

-- | A path: @$x@, a string, a parenthesised expression or a bareword.
pathArg :: Parser Surface
pathArg = do
  block <- asks inBlock
  when block (notFollowedBy (keyword "end"))
  arg <- spanned (SVar <$> (char '$' *> identifier))
    <|> spanned (SLit . LStr <$> stringLiteral)
    <|> between (symbol "(") (char ')') (withComma False expr)
    <|> spanned (SLit . LStr <$> some wordChar)
  sc
  pure arg

-- | @x y -> body@ means @\\x -> \\y -> body@. The body goes as far right as it
-- can.
--
-- Note: @f x@ starts like a lambda until the arrow fails to arrive. Look at 'try'.
lambda :: Parser Surface
lambda = spanned $ do
  names <- try (some (lexeme identifier) <* symbol "->")
  SLam names <$> binding names expr

-- | @let x = bound in body@.
letIn :: Parser Surface
letIn = spanned $ do
  lexeme (keyword "let")
  name <- lexeme identifier
  _ <- symbol "="
  bound <- expr
  lexeme (keyword "in")
  SLet name bound <$> binding [name] expr

-- | @do s; s; e end@. A statement is @x <- e@, @x = e@ or @e@, and the last
-- one is an expression. A name that a statement binds is bound for the
-- statements after it, so it is a function there, not a command. A @;@ may
-- come before @end@.
--
-- Note: inside a block, a bare @end@ ends a command line. Quote it to pass it.
doBlock :: Parser Surface
doBlock = spanned $ do
  lexeme (keyword "do")
  stmts <- local (\context -> context {inBlock = True, commaDelimiter = False}) statements
  keyword "end"
  case reverse stmts of
    StmtRun _ : _ -> pure (SDo stmts)
    _ -> fail "a block must end with an expression"
 where
  statements = do
    stmt <- statement
    let names = case stmt of
          StmtBind name _ -> [name]
          StmtLet name _ -> [name]
          StmtRun _ -> []
    more <- option [] $ do
      _ <- symbol ";"
      option [] (notFollowedBy (keyword "end") *> binding names statements)
    pure (stmt : more)
  statement =
        (StmtBind <$> try (lexeme identifier <* symbol "<-") <*> expr)
    <|> (StmtLet <$> try (lexeme identifier <* symbol "=" <* notFollowedBy (char '=')) <*> expr)
    <|> (StmtRun <$> expr)

-- | @with overlay do ... end@.
-- A bare reference resolves with pkg first: see "Lambdash.Desugar".
withBlock :: Parser Surface
withBlock = spanned $ do
  lexeme (keyword "with")
  overlay <- postfix
  SWith overlay <$> doBlock

-- | Any expression.
--
-- A trailing @&@ starts the whole pipeline before it as a background job:
-- @a | b &@ is @(a | b) &@.
expr :: Parser Surface
expr = letIn <|> lambda <|> pipeline
 where
  pipeline = do
    firstStage <- application
    whole <- stages firstStage
    background <- optional (try (symbol "&" <* notFollowedBy (char '&')))
    maybe (pure whole) (const (extend whole (SBg whole))) background
  stages left = option left $ do
    right <- symbol "|" *> (lambda <|> application)
    extend left (SPipe left right) >>= stages

-- | Parse one whole expression to core, without spans, or give a readable
-- error. This is for tests and tools: the REPL keeps the spans.
parseExpr :: String -> Either String Expr
parseExpr = parseExprIn AllBound

-- | The same, given what is bound.
parseExprIn :: Bound -> String -> Either String Expr
parseExprIn bound = fmap (stripLocations . desugar) . parseSurfaceIn bound

-- | Parse one whole expression to surface syntax.
parseSurfaceIn :: Bound -> String -> Either String Surface
parseSurfaceIn bound = first errorBundlePretty . runIn bound (sc *> expr <* eof)
