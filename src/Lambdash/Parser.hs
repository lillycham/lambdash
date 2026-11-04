-- | The expression parser. Syntax is provisional.
--
-- > x y -> body                 lambda, one or more parameters
-- > let x = 1 in x              let
-- > let rec f = x -> ... in f   recursive let; and joins a group
-- > f x y                       application, to the left
-- > {name = 1, ok = true}       record
-- > r.name                      select, with no space before the dot
-- > .name  .a.b                 accessor: \r -> r.name
-- > 1  "a"  true  false  ()  x  (e)  atoms
-- > git log --oneline $x        external command, when nothing binds the head
-- > source | stage | stage      pipeline, below application precedence
-- > do x <- e; f x end           a block of statements, run in order
-- > case m of Just x -> x end   pattern matching; arms joined by ;
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
    recGroup,
    doBlock,
    caseOf,
    casePattern,
    expr,
    parseExpr,
    parseSurfaceIn,
    Fixity (..),
    Assoc (..),
    Fixities,
    defaultFixities,
    runWith,
    operatorName,
    parenOperator,
    typeExpr,
    qualifiedType,
    declaration,
  )
where

import Control.Monad (when)
import Control.Monad.Reader (Reader, asks, local, runReader)
import Data.Bifunctor (first)
import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Void (Void)
import Lambdash.Desugar (desugar, stripLocations)
import Lambdash.Syntax
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, hspace, letterChar, lowerChar, space1, string, upperChar)
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
  , -- | Between @if@ and @else@, and between @case@ and @of@, the words
    -- @then@, @else@ and @of@ end a command line.
    inCondition :: Bool
  , -- | Each operator's precedence and associativity. An operator not here
    -- is @infixl 9@.
    contextFixities :: Fixities
  }

data Assoc = InfixL | InfixR | InfixN
  deriving (Eq, Show)

data Fixity = Fixity Assoc Int
  deriving (Eq, Show)

type Fixities = Map.Map Name Fixity

-- | The operator table, loosest first. Every operator binds tighter than
-- @|@ and looser than application.
defaultFixities :: Fixities
defaultFixities = Map.fromList $
  [(op, Fixity InfixR 1) | op <- [">>>", "<<<"]]
    <> [("||", Fixity InfixR 2), ("&&", Fixity InfixR 3)]
    <> [(op, Fixity InfixN 4) | op <- ["==", "!=", "<", "<=", ">", ">="]]
    <> [("<>", Fixity InfixR 5)]
    <> [(op, Fixity InfixL 6) | op <- ["+", "-"]]
    <> [(op, Fixity InfixL 7) | op <- ["*", "/", "%"]]

type Parser = ParsecT Void String (Reader ParseContext)

withComma :: Bool -> Parser a -> Parser a
withComma active = local (\context -> context {commaDelimiter = active})

-- | Run a parser, given what is bound.
runIn :: Bound -> Parser a -> String -> Either (ParseErrorBundle String Void) a
runIn = runWith defaultFixities

-- | The same, with the session's fixities.
runWith :: Fixities -> Bound -> Parser a -> String -> Either (ParseErrorBundle String Void) a
runWith fixities bound p input = runReader (runParserT p "<input>" input) (ParseContext bound False False False fixities)

operatorChars :: String
operatorChars = "!%&*+-/<=>?@^|~:"

-- | A run of operator characters that is an operator. The reserved ones
-- mean something else: =, ->, <-, |, &, ::, a lone :, and anything that
-- starts a comment.
operatorName :: Parser Name
operatorName = try $ do
  name <- some (satisfy (`elem` operatorChars))
  if name `elem` ["=", "->", "<-", "|", "&", "::", ":"] || take 2 name == "--"
    then fail ("reserved: " <> name)
    else pure name

-- | @(op)@: an operator as a name.
parenOperator :: Parser Name
parenOperator = try (symbol "(" *> lexeme operatorName <* char ')')

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
    "with",
    "if",
    "then",
    "else",
    "rec",
    "and",
    "case",
    "of"
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
  spanned (SLit LUnit <$ try (symbol "(" *> char ')'))
    <|> parenthesised
    <|> doBlock
    <|> withBlock
    <|> caseOf
    <|> spanned (SLit . LRef <$> flakeRef)
    <|> spanned (SLit <$> literal)
    <|> record
    <|> spanned (SList <$> between (symbol "[") (char ']') (withComma True expr `sepBy` symbol ","))
    <|> accessor
    <|> spanned (SVar <$> identifier)

-- | An expression in parentheses, an operator as a value, or a section.
-- @(-1)@ is a number and @(- 1)@ the section @x -> x - 1@.
parenthesised :: Parser Surface
parenthesised = spanned $ between (symbol "(") (char ')') (withComma False inner)
 where
  inner =
        (try (SVar <$> lexeme operatorName <* lookAhead (char ')')))
    <|> (try (notFollowedBy (char '-' *> satisfy isDigit)) *> try (SRightSection <$> lexeme operatorName <*> operators))
    <|> do
      e <- expr
      annotation <- optional (try (symbol ":" <* notFollowedBy (char ':')) *> typeExpr)
      section <- case annotation of
        Just _ -> pure Nothing
        Nothing -> optional (lexeme operatorName <* lookAhead (char ')'))
      pure $ case (annotation, section) of
        (Just written, _) -> SAnn e written
        (_, Just op) -> SLeftSection e op
        _ -> plain e
  plain e
    | Surface _ (SOp _ _ _) <- e, leftmostAccessor e = SAccessorSection e
    | Surface _ node <- e = node
  leftmostAccessor (Surface _ (SOp _ left _)) = leftmostAccessor left
  leftmostAccessor (Surface _ (SAccessor _)) = True
  leftmostAccessor _ = False

-- | An atom, then any number of @.label@, then skip what follows.
--
-- This is why 'atom' leaves trailing space alone: @r.a@ selects, but in @f .a@
-- the space means that @.a@ is an argument. If 'atom' ate the space, the two
-- would look the same here.
postfix :: Parser Surface
postfix = fst <$> postfixSpaced

-- | The same, and whether space followed it: x-1 subtracts, f -1 applies.
postfixSpaced :: Parser (Surface, Bool)
postfixSpaced = do
  e <- atom
  selected <- selects e
  before <- getOffset
  sc
  after <- getOffset
  pure (selected, after > before)
 where
  selects e = option e $ do
    field <- char '.' *> identifier
    extend e (SSelect e field) >>= selects

-- | One or more 'postfix', applied to the left: @f x y@ is @(f x) y@.
-- | A string in double quotes, with the escapes of Haskell.
stringLiteral :: Parser String
stringLiteral = char '"' *> manyTill L.charLiteral (char '"')

-- | A bareword. Space and @|"$;&@ end it, because the line needs them, and
-- so does a closing bracket that the word did not open: in @[a, b]@ the @]@
-- ends the list, but @clip[4PQ].webm@ and @f(1).txt@ are one word each.
-- A backslash keeps the next character in the word, as in @my\\ file@, as
-- tab completion writes it.
bareWord :: Parser String
bareWord = do
  comma <- asks commaDelimiter
  let plain open c =
        not (isSpace c) && c `notElem` ("|\"$;&\\" :: String) && not (comma && c == ',')
          && maybe True (\opener -> take 1 open == [opener]) (lookup c closers)
      -- The brackets the word has opened, innermost first. An escaped
      -- bracket is only a character.
      next open =
            (char '\\' *> anySingle >>= \c -> (c :) <$> option [] (next open))
        <|> (satisfy (plain open) >>= \c -> (c :) <$> option [] (next (track c open)))
  next []
 where
  closers = [(')', '('), ('}', '{'), (']', '[')]
  track c open
    | c `elem` map snd closers = c : open
    | c `elem` map fst closers = drop 1 open
    | otherwise = open

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
  bareWord

-- | One argument of an external command.
argument :: Parser Arg
argument = do
  block <- asks inBlock
  when block (notFollowedBy (keyword "end"))
  condition <- asks inCondition
  when condition (notFollowedBy (keyword "then" <|> keyword "else" <|> keyword "of"))
  AVar <$> (char '$' *> identifier)
    <|> AWord <$> stringLiteral
    <|> AWord <$> bareWord

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
  (fun, spaced) <- postfixSpaced
  slotted <- case fun of
    Surface _ (SVar name) -> do
      signature <- asks (signatureOf name . contextBound)
      maybe (pure []) slotArgs signature
    _ -> pure []
  args <- arguments spaced
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

-- | Arguments, while the next thing is not an operator. A minus starts an
-- argument only after a space and before a digit: f -1 is f (-1), and x-1
-- and x - 1 subtract.
arguments :: Bool -> Parser [Surface]
arguments spaced = option [] $ do
  next <- lookAhead (optional (satisfy (`elem` operatorChars)))
  case next of
    Just '-' | spaced -> do
      _ <- lookAhead (try (char '-' *> satisfy isDigit))
      (arg, spaced') <- postfixSpaced
      (arg :) <$> arguments spaced'
    Just _ -> pure []
    Nothing -> do
      (arg, spaced') <- postfixSpaced
      (arg :) <$> arguments spaced'

-- | Operands joined by operators, resolved by precedence. A lambda can be
-- the last operand: f >>> x -> x.
operators :: Parser Surface
operators = do
  leftmost <- operand
  -- An operator just before a closing parenthesis is a left section's.
  rest <- many ((,) <$> try (operatorToken <* notFollowedBy (char ')')) <*> operand)
  fixities <- asks contextFixities
  either fail pure (resolve fixities leftmost rest)
 where
  operand = lambda <|> application
  operatorToken = try (lexeme operatorName <* notFollowedBy (char '>'))

-- | Shunting-yard over one flat run of operators. An operator binds by its
-- fixity; a non-associative operator cannot chain, and two operators of one
-- precedence must agree on associativity.
resolve :: Fixities -> Surface -> [(Name, Surface)] -> Either String Surface
resolve fixities leftmost rest = go [leftmost] [] rest
 where
  fixity op = Map.findWithDefault (Fixity InfixL 9) op fixities
  go operands stack [] = finish operands stack
  go operands stack ((op, e) : more) = do
    (operands', stack') <- popWhile op operands stack
    go (e : operands') (op : stack') more
  popWhile op operands stack = case stack of
    top : below -> do
      let Fixity assocTop precTop = fixity top
          Fixity assocOp precOp = fixity op
      if precTop > precOp || (precTop == precOp && assocTop == InfixL && assocOp == InfixL)
        then popWhile op (reduce top operands) below
        else if precTop == precOp && (assocTop /= assocOp || assocOp == InfixN)
          then Left ("cannot mix " <> top <> " and " <> op <> " without parentheses")
          else pure (operands, stack)
    [] -> pure (operands, stack)
  finish operands [] = case operands of
    [e] -> pure e
    _ -> Left "an operator is missing an operand"
  finish operands (top : below) = finish (reduce top operands) below
  reduce op (right : left : others) = Surface (spanOf left right) (SOp op left right) : others
  reduce _ operands = operands
  spanOf (Surface (Span start _) _) (Surface (Span _ end) _) = Span start end

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
  condition <- asks inCondition
  when condition (notFollowedBy (keyword "then" <|> keyword "else" <|> keyword "of"))
  arg <- spanned (SVar <$> (char '$' *> identifier))
    <|> spanned (SLit . LStr <$> stringLiteral)
    <|> between (symbol "(") (char ')') (withComma False expr)
    <|> spanned (SLit . LStr <$> bareWord)
  sc
  pure arg

-- | @x y -> body@ means @\\x -> \\y -> body@. The body goes as far right as it
-- can. A parameter @_@ binds nothing: desugaring gives it a fresh name.
--
-- Note: @f x@ starts like a lambda until the arrow fails to arrive. Look at 'try'.
lambda :: Parser Surface
lambda = spanned $ do
  names <- try (some (lexeme parameter) <* symbol "->")
  SLam names <$> binding names expr
 where
  parameter = identifier <|> ("_" <$ try (char '_' <* notFollowedBy identifierChar))

-- | @if c then a else b@. The @else@ branch goes as far right as it can,
-- like a lambda's body.
ifThenElse :: Parser Surface
ifThenElse = spanned $ do
  lexeme (keyword "if")
  condition <- guarded expr
  lexeme (keyword "then")
  yes <- guarded expr
  lexeme (keyword "else")
  SIf condition yes <$> local (\c -> c {inCondition = False}) expr
 where
  guarded = local (\c -> c {inCondition = True})

-- | @let x = bound in body@, or @let rec f = x -> ... and g = y -> ... in
-- body@.
letIn :: Parser Surface
letIn = spanned $ do
  lexeme (keyword "let")
  recursive <- optional (lexeme (keyword "rec"))
  case recursive of
    Just () -> do
      group <- recGroup
      lexeme (keyword "in")
      SLetRec group <$> binding (map fst group) expr
    Nothing -> do
      name <- lexeme identifier <|> lexeme parenOperator
      _ <- symbol "="
      bound <- expr
      lexeme (keyword "in")
      SLet name bound <$> binding [name] expr

-- | The bindings of @let rec@, after @rec@: @f = x -> ... and g = y -> ...@.
-- Every name is bound in every bound expression, so @g x@ in @f@'s body is
-- a call, not a command, even before @g@ is defined. The group is parsed
-- twice for that: once to find the names, once with all of them bound.
--
-- Each bound expression must be a lambda, as in OCaml: evaluation is
-- strict, so @let rec x = x + 1@ could only loop.
recGroup :: Parser [(Name, Surface)]
recGroup = do
  -- The first pass treats every head as bound, as a name defined later
  -- would be. It only finds the names; if it cannot, the second pass binds
  -- each name for the bindings after it, and reports the error.
  names <- option [] (try (lookAhead (local (\c -> c {contextBound = AllBound}) (map fst <$> bindings []))))
  group <- binding names (bindings [])
  case [name | (name, bound) <- group, not (isLambda bound)] of
    name : _ -> fail ("let rec binds functions only: write " <> name <> " = x -> ...")
    [] -> pure group
 where
  bindings known = do
    name <- lexeme identifier <|> lexeme parenOperator
    _ <- symbol "=" <* notFollowedBy (char '=')
    bound <- binding (name : known) expr
    more <- option [] (lexeme (keyword "and") *> bindings (name : known))
    pure ((name, bound) : more)
  isLambda (Surface _ node) = case node of
    SLam _ _ -> True
    SAnn inner _ -> isLambda inner
    _ -> False

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
    <|> (StmtLet <$> try ((lexeme identifier <|> lexeme parenOperator) <* symbol "=" <* notFollowedBy (char '=')) <*> expr)
    <|> (StmtRun <$> expr)

-- | @case e of p -> a; q -> b end@. Each arm binds its pattern's names in
-- its body. As in a block, @;@ and a bare @end@ end a command line.
--
-- The scrutinee ends at @of@, which ends a command line there, as @then@
-- does after @if@.
caseOf :: Parser Surface
caseOf = spanned $ do
  lexeme (keyword "case")
  scrutinee <- local (\c -> c {inCondition = True}) expr
  lexeme (keyword "of")
  arms <- local (\context -> context {inBlock = True, commaDelimiter = False}) (arm `sepEndBy1` symbol ";")
  keyword "end"
  pure (SCase scrutinee arms)
 where
  arm = do
    p <- casePattern
    _ <- symbol "->"
    body <- binding (patternVars p) expr
    pure (p, body)

-- | A pattern: @_@, @x@, @Just x@, @Cons x (Cons y rest)@, @0@, @"s"@,
-- @true@, @()@ or @{name = p, size}@. A capitalised name is a constructor, and any other name
-- binds.
casePattern :: Parser Pattern
casePattern = (PCon <$> lexeme upperName <*> many patternAtom) <|> patternAtom

patternAtom :: Parser Pattern
patternAtom =
      (PWild <$ lexeme (try (char '_' <* notFollowedBy identifierChar)))
  <|> (PLit LUnit <$ try (symbol "(" *> symbol ")"))
  <|> recordPattern
  <|> between (symbol "(") (symbol ")") casePattern
  <|> (PLit . LStr <$> lexeme stringLiteral)
  <|> (PLit . LInt <$> try (lexeme signedInteger))
  <|> (PLit (LBool True) <$ lexeme (keyword "true"))
  <|> (PLit (LBool False) <$ lexeme (keyword "false"))
  <|> (PCon <$> lexeme upperName <*> pure [])
  <|> (PVar <$> lexeme lowerName)

-- | @{name = p, size}@: a field alone binds its own name. The pattern is
-- open, so the record may have more fields.
recordPattern :: Parser Pattern
recordPattern = PRecord <$> between (symbol "{") (symbol "}") (field `sepBy` symbol ",")
 where
  field = do
    name <- lexeme identifier
    (,) name <$> ((symbol "=" *> casePattern) <|> pure (PVar name))

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
expr = letIn <|> ifThenElse <|> lambda <|> pipeline
 where
  pipeline = do
    firstStage <- operators
    whole <- stages firstStage
    -- A lone & backgrounds; && is an operator.
    background <- optional (try (char '&' <* notFollowedBy (satisfy (`elem` operatorChars))) <* sc)
    maybe (pure whole) (const (extend whole (SBg whole))) background
  -- A lone | is a pipe; || is an operator.
  stages left = option left $ do
    right <- try (char '|' <* notFollowedBy (satisfy (`elem` operatorChars)) <* sc) *> operators
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

-- | A type, as in a class's method signatures:
--
-- > a -> b                      a function, to the right
-- > Int  String  ()  [a]        constructors, lists
-- > {name : String | r}         a record, open with a tail
-- > Comp a  Job a  Stream i o r  Stage i o  Collector i o
-- > Maybe Int  Either e a        a declared type and its arguments
typeExpr :: Parser SType
typeExpr = do
  t <- appliedType
  option t (STFun t <$> (symbol "->" *> typeExpr))
 where
  appliedType = do
    name <- lookAhead (optional (try (upperName <* notFollowedBy identifierChar)))
    case name >>= (`lookup` arities) of
      Just n -> do
        _ <- lexeme upperName
        -- A brace group right after is a row when the arguments still
        -- follow it: Comp {console} a. Otherwise it is the record that
        -- Comp {name : String} gives.
        let applied = maybe "" id name
            withRow = try ((\r args -> STApp applied (Just r) args) <$> effectRow <*> count n typeAtom)
        if applied `elem` ["Comp", "Job", "Stream"]
          then withRow <|> (STApp applied Nothing <$> count n typeAtom)
          else STApp applied Nothing <$> count n typeAtom
      -- A declared type takes its arguments: Maybe Int, Either e a.
      Nothing | Just con <- name -> do
        _ <- lexeme upperName
        args <- many typeAtom
        pure (if null args then STCon con else STApp con Nothing args)
      Nothing -> typeAtom
  arities = [("Comp", 1), ("Job", 1), ("Stream", 3), ("Stage", 2), ("Collector", 2)]

-- | @{exec git, console | e}@. Labels are as @:t@ prints them.
effectRow :: Parser SRow
effectRow = between (symbol "{") (symbol "}") $ do
  labels <- lexeme effectName `sepBy` symbol ","
  rest <- optional (symbol "|" *> lexeme lowerName)
  pure (SRow labels rest)
 where
  effectName =
        (\name -> "exec " <> name) <$> (lexeme (keyword "exec") *> some (satisfy (\c -> not (isSpace c) && c `notElem` (",|}" :: String))))
    <|> choice [word <$ keyword word | word <- ["console", "env", "fs", "cwd"]]

-- | A type that needs no parentheses.
typeAtom :: Parser SType
typeAtom =
      (STCon "()" <$ try (symbol "(" *> symbol ")"))
  <|> between (symbol "(") (symbol ")") typeExpr
  <|> (STList <$> between (symbol "[") (symbol "]") typeExpr)
  <|> recordType
  <|> (STVar <$> lexeme lowerName)
  <|> (STCon <$> lexeme upperName)
 where
  recordType = between (symbol "{") (symbol "}") $ do
    fields <- ((,) <$> lexeme identifier <*> (symbol ":" *> typeExpr)) `sepBy` symbol ","
    rest <- optional (symbol "|" *> lexeme lowerName)
    pure (STRecord fields rest)

lowerName :: Parser Name
lowerName = try (lookAhead lowerChar *> identifier)

upperName :: Parser Name
upperName = try (lookAhead upperChar *> identifier)

-- | @class ... where ...@ or @instance ... where ...@. Several methods go in
-- a block: @where do m : t; n : u end@.
declaration :: Parser Declaration
declaration = classDeclaration <|> instanceDeclaration <|> dataDeclaration
 where
  start word = try (lexeme (keyword word) <* lookAhead (upperChar <|> char '('))
  classDeclaration = do
    start "class"
    supers <- context
    cls <- lexeme upperName
    var <- lexeme lowerName
    lexeme (keyword "where")
    items <- classMembers
    pure (DeclClass supers cls var [x | Left x <- items] [x | Right x <- items])
  -- data List a = Nil | Cons a (List a)
  dataDeclaration = do
    start "data"
    typeName <- lexeme upperName
    params <- many (lexeme lowerName)
    _ <- symbol "=" <* notFollowedBy (char '=')
    DeclData typeName params <$> (((,) <$> lexeme upperName <*> many typeAtom) `sepBy1` symbol "|")
  instanceDeclaration = do
    start "instance"
    requires <- context
    cls <- lexeme upperName
    headTy <- typeAtom
    lexeme (keyword "where")
    DeclInstance requires cls headTy <$> members definition
  context = option [] (try (predicates <* symbol "=>"))
  predicates = between (symbol "(") (symbol ")") (predicate `sepBy1` symbol ",") <|> ((: []) <$> predicate)
  predicate = SPred <$> lexeme upperName <*> typeAtom
  name = lexeme identifier <|> lexeme parenOperator
  signature = (,) <$> try (name <* symbol ":" <* notFollowedBy (char ':')) <*> typeExpr
  definition = (,) <$> try (name <* symbol "=" <* notFollowedBy (char '=')) <*> local (\c -> c {inBlock = True}) expr
  -- A signature binds its method for the members after it, so that a
  -- default can use it: size x is a method call, not a command.
  classMembers =
        (lexeme (keyword "do") *> local (\c -> c {inBlock = True}) classItems <* lexeme (keyword "end"))
    <|> ((: []) <$> classItem)
  classItem = Left <$> signature <|> Right <$> definition
  classItems = do
    item <- classItem
    let names = either (pure . fst) (const []) item
    more <- option [] (symbol ";" *> option [] (notFollowedBy (keyword "end") *> binding names classItems))
    pure (item : more)
  members item =
        (lexeme (keyword "do") *> (item `sepEndBy1` symbol ";") <* lexeme (keyword "end"))
    <|> ((: []) <$> item)

-- | A type with an optional context: @Eq a => a -> Bool@.
qualifiedType :: Parser ([SPred], SType)
qualifiedType = (,) <$> option [] (try (predicates <* symbol "=>")) <*> typeExpr
 where
  predicates = between (symbol "(") (symbol ")") (predicate `sepBy1` symbol ",") <|> ((: []) <$> predicate)
  predicate = SPred <$> lexeme upperName <*> typeAtom
