-- | The REPL as a pure function: a session and a line give a new session and
-- some text. The loop in @app/Main.hs@ only moves text, so everything here can
-- be tested without a terminal.
module Lambdash.Repl
  ( Session (..)
  , initial
  , sessionPrompt
  , needsMore
  , arrive
  , notices
  , Command (..)
  , command
  , parseCommand
  , parseCommandWith
  , renderType
  , renderEffectsType
  , renderError
  , Step (..)
  , step
  , lineFailure
  , declareIn
  , runScript
  , startupFile
  , Entry
  ) where

import Control.Monad (filterM)
import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import Data.Bifunctor (first)
import Data.Char (chr, isAlphaNum, ord)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.List (intercalate, isPrefixOf, nub, partition)
import Control.Concurrent.STM
import Control.Exception (ArithException, ErrorCall (..), PatternMatchFail (..), SomeException, catch, fromException, throwIO)
import Control.Exception qualified as Exception
import Data.Char (digitToInt, isDigit)
import Lambdash.Stream (StreamError (..))
import Lambdash.Eval
import Lambdash.Infer
import Lambdash.Desugar (desugar, renderCore)
import Lambdash.Parser
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Control.Monad (when)
import System.Posix.IO (stdInput)
import System.Posix.Process (getProcessGroupID)
import System.Posix.Terminal (queryTerminal)
import Lambdash.Nix (resolveDev)
import Lambdash.Overlay
import Lambdash.Trust (autoName, flakeHash, isAllowed)
import Lambdash.Trust qualified as Trust
import Lambdash.Runtime
import Lambdash.Process (Status (..), giveTerminal, programExists)
import Lambdash.Unify
import Lambdash.Classes (ClassEnv (..), ClassInfo (..), DataInfo (..), Instance (..), addClass, addData, addInstance, instanceName, lookupClass, lookupData, prelude, retireData, retiredName)
import Lambdash.Help qualified as Help
import Lambdash.Declare (Declared (..), declare)
import Lambdash.Find (Found (..), findByType)
import Lambdash.Syntax (Declaration (..), SPred (..), SType (..), renderPattern)
import Data.Map.Lazy qualified as Map.Lazy
import Text.Megaparsec ((<|>), eof, errorBundlePretty, lookAhead, many, notFollowedBy, optional, satisfy, some, try)
import Text.Megaparsec.Char (char)
import System.Directory (XdgDirectory (..), canonicalizePath, doesFileExist, getCurrentDirectory, getHomeDirectory, getXdgDirectory, makeAbsolute)
import System.FilePath (takeDirectory, (</>))

import System.Environment (lookupEnv)

-- | What persists between lines. A name has its type in one and its value in
-- the other, so an update has to change both.
data Session = Session
  { sessionTypes :: Env
  , sessionValues :: Scope
  , -- | The active environments.
    sessionShell :: ShellState
  , -- | Where Nix GC roots go. Without one, @pkg@ and @dev@ root nothing.
    sessionRoots :: Maybe FilePath
  , -- | Stopped jobs, oldest first.
    sessionJobs :: [Entry]
  , -- | Whether @:q@ has already warned about stopped jobs.
    sessionWarned :: Bool
  , -- | Directories whose flake activated on arrival, with the hash approved.
    sessionFlakes :: Map.Map FilePath String
  , -- | Directories and hashes already reported as not allowed.
    sessionNotified :: Set.Set (FilePath, String)
  , -- | Operator fixities: the table, and the session's fixity lines.
    sessionFixities :: Fixities
  , -- | The classes and instances: the prelude's, and the session's
    -- declarations.
    sessionClasses :: ClassEnv
  , -- | Signatures waiting for their binding: @f : t@, then @f = e@.
    sessionSignatures :: Map.Map Name ([SPred], SType)
  , -- | Where a relative import starts: the directory of the file that the
    -- lines come from. Without one, the current directory.
    sessionDirectory :: Maybe FilePath
  , -- | The modules being loaded, innermost first, so that a cycle stops.
    sessionImporting :: [FilePath]
  }

-- | A job in the table: a line that Ctrl-Z stopped, or a job that @&@
-- started. Either is still running on its own thread, or parked there with
-- its processes, pipes and workers as they were.
data Entry = Entry
  { -- | The number in the table. A line takes one only when it stops, so
    -- that the jobs it starts with @&@ get the lowest numbers.
    entryNumber :: Int
  , entryJob :: Job
  , -- | For a line: where its result arrives, and what the line does with it,
    -- given the session at that time.
    entryLine :: Maybe (TMVar (Either SomeException Value), Session -> Value -> IO Step)
  , -- | Where the line registers the jobs it starts with @&@.
    entryTable :: Maybe JobTable
  , -- | The state last reported, so that each change is reported once.
    entrySeen :: JobState
  }

-- | The session before any input.
initial :: Session
initial = Session typeEnv scope emptyState Nothing [] False Map.empty Set.empty defaultFixities prelude Map.empty Nothing []

-- | Does this input open more @do@ blocks or @case@s than it closes? The REPL then reads
-- another line. Only whole words count, outside string literals and
-- comment lines. A line whose first word starts with @--@ is a comment. A
-- @--@ later in a line can be a word of an external command, as in
-- @git log -- path@, so it does not end the line here.
needsMore :: String -> Bool
needsMore input = depth (tokens (withoutStrings (unlines (filter (not . comment) (lines input))))) > 0
 where
  comment line = take 2 (dropWhile (`elem` (" \t" :: String)) line) == "--"
  withoutStrings [] = []
  withoutStrings ('"' : rest) = ' ' : withoutStrings (skipString rest)
  withoutStrings (c : rest) = c : withoutStrings rest
  skipString ('\\' : _ : rest) = skipString rest
  skipString ('"' : rest) = rest
  skipString (_ : rest) = skipString rest
  skipString [] = []
  tokens = words . map (\c -> if c `elem` (";(){}[]|," :: String) then ' ' else c)
  depth = foldl (\n token -> case token of
    "do" -> n + 1
    "case" -> n + 1
    "end" -> n - 1
    _ -> n) (0 :: Int)

-- | What the prompt should say: the active environments, and any shadowing.
-- Only the overlays active in this directory appear.
sessionPrompt :: FilePath -> Session -> String
sessionPrompt cwd = prompt . inScope cwd . sessionShell

-- | What one line asks for.
data Command
  = -- | Nothing, or only a comment.
    Blank
  | -- | @:q@.
    Exit
  | -- | @:t expr@.
    TypeOf Expr
  | -- | @:core expr@: what the expression desugars to. Nothing runs.
    ShowCore Expr
  | -- | @:elab expr@: the code a line would run, with its dictionaries.
    -- @:elab x = expr@: the code a binding would hold. Nothing runs.
    ShowElab (Maybe Name) Expr
  | -- | @:env@ lists the active environments and their patches. @:env NAME@
    -- shows a variable, and each layer that touched it.
    ShowEnv (Maybe String)
  | -- | @x = expr@.
    Bind Name Expr
  | -- | @let rec f = x -> ... and g = y -> ...@, without @in@: bind the
    -- group's names for the lines after.
    BindRec [(Name, Expr)]
  | -- | @x <- expr@.
    BindRun Name Expr
  | -- | @expr@.
    Run Expr
  | -- | @jobs@, when nothing binds it.
    ListJobs
  | -- | @fg@, or @fg 2@, when nothing binds it.
    Foreground (Maybe Int)
  | -- | @bg@, or @bg 2@: continue a stopped job without the terminal.
    Background (Maybe Int)
  | -- | @allow@: activate this directory's flake now and on every arrival,
    -- while @flake.nix@ and @flake.lock@ stay as they are.
    Allow
  | -- | @deny@: withdraw that.
    Deny
  | -- | @infixl 6 <+>@: how later lines parse an operator.
    SetFixity Fixity [Name]
  | -- | @class ...@ or @instance ...@.
    Declare Declaration
  | -- | @f : Eq a => a -> Bool@: the signature of the next @f = e@.
    Signature Name [SPred] SType
  | -- | @:find Ord a => [a] -> [a]@: the names whose type is at least as
    -- general as this one.
    Find [SPred] SType
  | -- | @:help@, @:help NAME@ or @:help TOPIC@.
    Help (Maybe String)
  | -- | @:elab instance ...@: an instance's dictionary, as code. Nothing is
    -- declared.
    ShowElabDeclaration Declaration
  | -- | @import "./lib.lsh"@ brings a module's names in as they are, and
    -- @tab = import "./lib.lsh"@ brings them in as @tab.name@.
    Import (Maybe Name) FilePath
  deriving (Eq, Show)

-- | One line.
--
-- Note: @x = 1@ starts like the expression @x@ until the @=@ arrives. Reserved
-- words help too: @keyword ":t"@ works here.
command :: Parser Command
command =
      (Blank <$ eof)
  <|> (Exit <$ lexeme (keyword ":q"))
  <|> (TypeOf <$> (lexeme (keyword ":t") *> core))
  <|> (ShowCore <$> (lexeme (keyword ":core") *> core))
  <|> try (lexeme (keyword ":elab") *> (ShowElabDeclaration <$> declaration))
  <|> (lexeme (keyword ":elab") *> (ShowElab <$> optional (try (name <* symbol "=" <* notFollowedBy (char '='))) <*> core))
  <|> (ShowEnv <$> (lexeme (keyword ":env") *> optionalName))
  <|> (uncurry Find <$> (lexeme (keyword ":find") *> qualifiedType))
  <|> (Help <$> (lexeme (keyword ":help" <|> keyword ":h") *> optionalName))
  <|> fixityLine
  -- Only a quoted path, and nothing after it, makes an import: import
  -- shot.png is still ImageMagick.
  <|> try (Import <$> optional (try (name <* symbol "=" <* notFollowedBy (char '='))) <*> (lexeme (keyword "import") *> lexeme stringLiteral <* eof))
  <|> (Declare <$> declaration)
  <|> (uncurry . Signature <$> try (name <* symbol ":" <* notFollowedBy (char ':')) <*> qualifiedType)
  <|> (Bind <$> try (name <* symbol "=" <* notFollowedBy (char '=')) <*> core)
  -- With in, it is an expression, for Run.
  <|> (BindRec . map (fmap desugar) <$> try (lexeme (keyword "let") *> lexeme (keyword "rec") *> recGroup <* lookAhead eof))
  <|> (BindRun <$> try (name <* symbol "<-") <*> core)
  <|> (Run <$> core)
 where
  -- The checker sees core, with the spans kept for its messages.
  core = desugar <$> expr
  -- x, or an operator in parentheses: (<+>) = a b -> a <> b.
  name = lexeme identifier <|> lexeme parenOperator
  -- infixl 6 <+> <->. Only a precedence and operators after it make this a
  -- fixity line, so infix is still a name.
  fixityLine = try $ do
    assoc <- (InfixL <$ lexeme (keyword "infixl")) <|> (InfixR <$ lexeme (keyword "infixr")) <|> (InfixN <$ lexeme (keyword "infix"))
    level <- lexeme (digitToInt <$> satisfy isDigit)
    SetFixity (Fixity assoc level) <$> some (lexeme operatorName)

-- A variable name is any run of characters up to a space.
optionalName :: Parser (Maybe String)
optionalName = do
  name <- lexeme (many (satisfy (\c -> c /= ' ' && c /= '\t')))
  pure (if null name then Nothing else Just name)

-- | Parse one whole line, or give a readable error.
--
-- @jobs@ and @fg@ are commands only when nothing binds them, like an external
-- command. They act on the shell, so they cannot be ordinary values.
parseCommand :: Bound -> String -> Either String Command
parseCommand = parseCommandWith defaultFixities

-- | The same, with the session's fixities.
parseCommandWith :: Fixities -> Bound -> String -> Either String Command
parseCommandWith fixities bound input = case words input of
  ["jobs"] | free "jobs" -> Right ListJobs
  ["allow"] | free "allow" -> Right Allow
  ["deny"] | free "deny" -> Right Deny
  ["bg"] | free "bg" -> Right (Background Nothing)
  ["bg", number] | free "bg", Just n <- jobNumberOf number -> Right (Background (Just n))
  ["fg"] | free "fg" -> Right (Foreground Nothing)
  ["fg", number] | free "fg", Just n <- jobNumberOf number -> Right (Foreground (Just n))
  _ -> first errorBundlePretty (runWith fixities bound (sc *> command <* eof) input)
 where
  -- A word is a command when nothing binds it, or when it is still the
  -- builtin: fg is also a function, and fg j applies it.
  free name = case bound of
    AllBound -> False
    BoundNames names -> Set.notMember name names
    BoundWith names builtin -> Set.notMember name names || Set.member name builtin
  jobNumberOf ('%' : digits) = jobNumberOf digits
  jobNumberOf digits
    | not (null digits) && all isDigit digits = Just (read digits)
    | otherwise = Nothing

-- | Show a type. Unknowns get letters in first-seen order, so the numbers from
-- the checker never reach the user: @a -> a@, @(a -> b) -> a -> b@,
-- @{size : a | b} -> a@, @{a : Int, b : Bool}@, @{}@.
--
-- Note: an unknown can be in a row tail as well as in a type, and they share
-- letters. Only the left of an arrow needs parentheses.
renderType :: Ty -> String
renderType ty = evalState (renderTy (hideEffects ty)) Map.empty

-- | Effect rows appear only when the user asks for a type.
renderEffectsType :: Ty -> String
renderEffectsType ty = evalState (renderTy ty) Map.empty

hideEffects :: Ty -> Ty
hideEffects (TFun a b) = TFun (hideEffects a) (hideEffects b)
hideEffects (TQualified _ ty) = hideEffects ty
hideEffects (TClass ps ty) = TClass ps (hideEffects ty)
hideEffects (TList ty) = TList (hideEffects ty)
hideEffects (TData name ts) = TData name (map hideEffects ts)
hideEffects (TStage i o) = TStage (hideEffects i) (hideEffects o)
hideEffects (TCollector i o) = TCollector (hideEffects i) (hideEffects o)
hideEffects (TComp _ t) = TComp noEffects (hideEffects t)
hideEffects (TJob _ t) = TJob noEffects (hideEffects t)
hideEffects (TStream _ i o r) = TStream noEffects (hideEffects i) (hideEffects o) (hideEffects r)
hideEffects (TRecord row) = TRecord (hideRow row)
 where
  hideRow (RExtend label ty rest) = RExtend label (hideEffects ty) (hideRow rest)
  hideRow rest = rest
hideEffects ty = ty

-- The same name table serves types, row tails and both sides of an error.
type TypeNames = Map.Map Var String

renderVar :: Var -> State TypeNames String
renderVar var = do
  names <- get
  case Map.lookup var names of
    Just name -> pure name
    Nothing -> do
      let (suffix, letter) = Map.size names `divMod` 26
          name = [chr (ord 'a' + letter)] <> if suffix == 0 then "" else show suffix
      put (Map.insert var name names)
      pure name

renderTy :: Ty -> State TypeNames String
renderTy (TVar var) = renderVar var
renderTy (TCon name) = pure name
renderTy (TList ty) = do
  text <- renderTy ty
  pure ("[" <> text <> "]")
renderTy (TData name args) = do
  parts <- mapM renderTy args
  let argument ty text = case ty of
        TData _ (_ : _) -> "(" <> text <> ")"
        TFun _ _ -> "(" <> text <> ")"
        TComp _ _ -> "(" <> text <> ")"
        TJob _ _ -> "(" <> text <> ")"
        TStream _ _ _ _ -> "(" <> text <> ")"
        TStage _ _ -> "(" <> text <> ")"
        TCollector _ _ -> "(" <> text <> ")"
        _ -> text
  pure (unwords (name : zipWith argument args parts))
renderTy (TStage i o) = renderAdapter "Stage" i o
renderTy (TCollector i o) = renderAdapter "Collector" i o
-- One context for both kinds of condition: class predicates first, then
-- effect equalities. A lone predicate needs no parentheses: @Eq a => a -> a@.
renderTy (TQualified constraints ty) = renderContext [] constraints ty
renderTy (TClass preds ty) = renderContext preds [] ty
renderTy (TFun argument result) = do
  left <- renderTy argument
  right <- renderTy result
  let parameter = case argument of
        TFun _ _ -> "(" <> left <> ")"
        _ -> left
  pure (parameter <> " -> " <> right)
renderTy (TComp eff result) = do
  effects <- renderEffects eff
  text <- renderTy result
  pure $ case result of
    TFun _ _ -> "Comp" <> effects <> " (" <> text <> ")"
    TComp _ _ -> "Comp" <> effects <> " (" <> text <> ")"
    TJob _ _ -> "Comp" <> effects <> " (" <> text <> ")"
    TStream _ _ _ _ -> "Comp" <> effects <> " (" <> text <> ")"
    TStage _ _ -> "Comp" <> effects <> " (" <> text <> ")"
    TCollector _ _ -> "Comp" <> effects <> " (" <> text <> ")"
    _ -> "Comp" <> effects <> " " <> text
renderTy (TJob eff result) = do
  effects <- renderEffects eff
  text <- renderTy result
  pure $ case result of
    TFun _ _ -> "Job" <> effects <> " (" <> text <> ")"
    TComp _ _ -> "Job" <> effects <> " (" <> text <> ")"
    TStream _ _ _ _ -> "Job" <> effects <> " (" <> text <> ")"
    TJob _ _ -> "Job" <> effects <> " (" <> text <> ")"
    _ -> "Job" <> effects <> " " <> text
renderTy (TStream eff input output result) = do
  effects <- renderEffects eff
  parts <- mapM renderTy [input, output, result]
  let argument ty text = case ty of
        TFun _ _ -> "(" <> text <> ")"
        TComp _ _ -> "(" <> text <> ")"
        TJob _ _ -> "(" <> text <> ")"
        TStream _ _ _ _ -> "(" <> text <> ")"
        TStage _ _ -> "(" <> text <> ")"
        TCollector _ _ -> "(" <> text <> ")"
        _ -> text
  pure ("Stream" <> effects <> " " <> unwords (zipWith argument [input, output, result] parts))
renderTy (TRecord row) = do
  fields <- renderRow row
  pure ("{" <> fields <> "}")

renderContext :: [Pred] -> [EffConstraint] -> Ty -> State TypeNames String
renderContext preds constraints ty = case ty of
  TClass more inner -> renderContext (preds <> more) constraints inner
  TQualified more inner -> renderContext preds (constraints <> more) inner
  _ -> do
    classes' <- mapM renderPred preds
    effects <- mapM renderConstraint constraints
    text <- renderTy ty
    pure $ case classes' <> effects of
      [] -> text
      [single] | null constraints -> single <> " => " <> text
      conditions -> "(" <> intercalate ", " conditions <> ") => " <> text

renderPred :: Pred -> State TypeNames String
renderPred (IsIn cls ty) = do
  text <- renderTy ty
  pure $ cls <> " " <> case ty of
    TVar _ -> text
    TCon _ -> text
    TList _ -> text
    TRecord _ -> text
    _ -> "(" <> text <> ")"

renderAdapter :: String -> Ty -> Ty -> State TypeNames String
renderAdapter name input output = do
  parts <- mapM renderTy [input, output]
  let argument ty text = case ty of
        TFun _ _ -> "(" <> text <> ")"
        TComp _ _ -> "(" <> text <> ")"
        TJob _ _ -> "(" <> text <> ")"
        TStream _ _ _ _ -> "(" <> text <> ")"
        TStage _ _ -> "(" <> text <> ")"
        TCollector _ _ -> "(" <> text <> ")"
        _ -> text
  pure (name <> " " <> unwords (zipWith argument [input, output] parts))

renderEffects :: EffRow -> State TypeNames String
renderEffects row@(EffRow labels vars)
  | row == noEffects = pure ""
  | otherwise = do
      tails <- mapM renderVar (Set.toList vars)
      let label Console = "console"
          label (Exec name) = "exec " <> name
          label Environment = "env"
          label Filesystem = "fs"
          label WorkingDirectory = "cwd"
          fixed = intercalate ", " (map label (Set.toList labels))
          open = if null tails then "" else " | " <> intercalate " + " tails
      pure (" {" <> fixed <> open <> "}")

renderConstraint :: EffConstraint -> State TypeNames String
renderConstraint (EffEqual left right) = do
  a <- renderEffects left
  b <- renderEffects right
  let row "" = "{}"
      row (' ' : rest) = rest
      row text = text
  pure (row a <> " = " <> row b)

renderRow :: Row -> State TypeNames String
renderRow REmpty = pure ""
renderRow (RVar var) = (" | " <>) <$> renderVar var
renderRow (RExtend label ty rest) = do
  field <- renderTy ty
  tailText <- case rest of
    RExtend _ _ _ -> (", " <>) <$> renderRow rest
    _ -> renderRow rest
  pure (label <> " : " <> field <> tailText)

-- | Say what went wrong, in words for a person who typed a line.
--
-- > unbound name: x
-- > type mismatch: Int and Bool
-- > infinite type: a and a -> b
-- > no field: nmae
renderError :: InferError -> String
renderError (Unbound name) = "unbound name: " <> name
renderError (NoInstance p) = "no instance: " <> evalState (renderPred p) Map.empty
renderError (Ambiguous p) = "ambiguous type: " <> evalState (renderPred p) Map.empty
renderError (BadDeclaration message) = message
renderError (BadPattern message) = message
renderError (NotCovered p) = "the case does not cover " <> renderPattern p
renderError BackgroundCwd = "a job in the background cannot change the directory: run cd in the foreground"
renderError (SignatureNeeds p) = "the binding needs " <> evalState (renderPred p) Map.empty <> ", and its signature does not give it"
renderError (Unentailed p) = "the instance needs " <> evalState (renderPred p) Map.empty <> ", and its context does not give it"
renderError (Unify err) = case err of
  Mismatch left right -> renderPair "type mismatch: " left right <> retiredNote (renderPair "" left right)
  Infinite var ty -> renderPair "infinite type: " (TVar var) ty
  InfiniteRow var row ->
    renderPair "infinite row: " (TRecord (RVar var)) (TRecord row)
  MissingField label _ -> "no field: " <> label
  EffectMismatch left right -> renderEffectError "effect mismatch:" left right
  InconsistentEffects constraints -> flip evalState Map.empty $ do
    text <- mapM renderConstraint constraints
    pure ("inconsistent effect requirements: " <> intercalate ", " text)

renderEffectError :: String -> EffRow -> EffRow -> String
renderEffectError message left right = flip evalState Map.empty $ do
  l <- renderEffects left
  r <- renderEffects right
  pure (message <> (if null l then " {}" else l) <> " and" <> (if null r then " {}" else r))

renderPair :: String -> Ty -> Ty -> String
renderPair message left right = flip evalState Map.empty $ do
  leftText <- renderTy left
  rightText <- renderTy right
  pure (message <> leftText <> " and " <> rightText)

-- | Keep what a line did to the environments. Only a line that succeeded gets
-- here: a failure returns the old session, so activation is transactional.
--
-- Shadowing that the line introduced is reported once here, and stays in the
-- prompt for as long as it lasts.
--
-- A job resumed with @fg@ started from the environments of its own line. If
-- they have changed since, its changes are dropped rather than undoing the
-- later ones.
committed :: Runtime -> ShellState -> Session -> String -> IO Step
committed runtime start next text = do
  state <- readIORef (runtimeState runtime)
  cwd <- getCurrentDirectory
  let before = shadows (inScope cwd (sessionShell next))
      warnings = [renderShadow shadow | shadow <- shadows (inScope cwd state), shadow `notElem` before]
      output extra = intercalate "\n" (filter (not . null) (extra <> [text]))
  pure $ if state == start then Continue next (output [])
    else if sessionShell next /= start
      then Continue next (output ["warning: the environments changed while this job was stopped, so its changes to them are dropped"])
      else Continue next {sessionShell = state} (output warnings)

-- | Run a line's computation on its own thread, so that Ctrl-Z can park it.
-- Then wait for it, as 'awaitEntry' does.
foreground :: Session -> String -> Runtime -> Value -> (Runtime -> Session -> Value -> IO Step) -> IO Step
foreground session line runtime0 value finish = do
  table <- JobTable <$> newIORef (nextNumber session) <*> newIORef []
  let runtime = runtime0 {runtimeLine = line, runtimeTable = Just table}
  (job, result) <- startJob runtime 0 line True (`runComp` value) summarise
  awaitEntry session (Entry 0 job (Just (result, finish runtime)) (Just table) Running)

nextNumber :: Session -> Int
nextNumber session = 1 + maximum (0 : map entryNumber (sessionJobs session))

-- | Wait until a job ends or stops. A stopped job joins the table, and the
-- jobs that its line started with @&@ join it either way.
awaitEntry :: Session -> Entry -> IO Step
awaitEntry session entry = do
  let job = entryJob entry
  outcome <- case entryLine entry of
    Just (result, finish) -> fmap (Left . (,) finish) <$> awaitDone job (readTMVar result)
    Nothing -> fmap Right <$> awaitDone job (readTMVar (jobOutcome job))
  -- A job that was running in the background can end just as fg hands it the
  -- terminal, and then nothing hands it back. Take it back in every case.
  reclaimTerminal
  current <- adopt (entryTable entry) session
  case outcome of
    Nothing ->
      let number = if entryNumber entry == 0 then nextNumber current else entryNumber entry
          stopped = entry {entryNumber = number, entrySeen = Stopped}
      in pure (Continue current {sessionJobs = sessionJobs current <> [stopped], sessionWarned = False}
        (describeEntry Stopped stopped))
    Just (Left (finish, Right value)) -> finish current value
    Just (Left (_, Left err)) -> maybe (throwIO err) (pure . Continue current) (lineFailure err)
    Just (Right summary) -> do
      markCollected job
      pure (Continue current (either id id summary))

-- | Move the jobs that a line started with @&@ into the table.
adopt :: Maybe JobTable -> Session -> IO Session
adopt Nothing session = pure session
adopt (Just table) session = do
  started <- atomicModifyIORef' (tableNew table) (\jobs -> ([], jobs))
  let entries = [Entry (jobNumber job) job Nothing Nothing Running | job <- reverse started]
  pure session {sessionJobs = sessionJobs session <> entries}

reclaimTerminal :: IO ()
reclaimTerminal = do
  tty <- queryTerminal stdInput
  when tty (getProcessGroupID >>= giveTerminal "REPL reclaims the terminal")

describeEntry :: JobState -> Entry -> String
describeEntry state entry =
  "[" <> show (entryNumber entry) <> "] " <> word state <> "  " <> jobLine job
 where
  job = entryJob entry
  word Running = "running"
  word Stopped = "stopped"
  word Done = "done"

-- | What jobs did since the last prompt. A job that ended gives its result
-- now, unless wait or fg took it: a line's in the session as it is now, as
-- the line would have shown it. A job that stopped is reported once.
notices :: Session -> IO (Session, [String])
notices session = go session {sessionJobs = []} [] (sessionJobs session)
 where
  go current messages [] = pure (current, reverse messages)
  go current0 messages (entry : rest) = do
    current <- adopt (entryTable entry) current0
    let job = entryJob entry
        label = "[" <> show (entryNumber entry) <> "] "
        keep e = current {sessionJobs = sessionJobs current <> [e]}
    state <- atomically (jobState job)
    collected <- readTVarIO (jobCollected job)
    case state of
      Done | collected -> go current messages rest
      Done -> case entryLine entry of
        Just (result, finish) -> do
          value <- atomically (readTMVar result)
          case value of
            Right v -> do
              step' <- finish current v
              case step' of
                Continue next text -> go next (lines' [label <> "done  " <> jobLine job, text] : messages) rest
                Failed text -> go current ((label <> "failed  " <> jobLine job <> ": " <> text) : messages) rest
                Quit -> go current messages rest
            Left err -> case lineFailure err of
              Just message -> go current ((label <> "failed  " <> jobLine job <> ": " <> message) : messages) rest
              Nothing -> throwIO err
        Nothing -> do
          summary <- atomically (readTMVar (jobOutcome job))
          let text = case summary of
                Right shown -> lines' [label <> "done  " <> jobLine job, shown]
                Left message -> label <> "failed  " <> jobLine job <> ": " <> message
          go current (text : messages) rest
      Stopped | entrySeen entry /= Stopped ->
        go (keep entry {entrySeen = Stopped}) (describeEntry Stopped entry : messages) rest
      _ -> go (keep entry {entrySeen = state}) messages rest
  lines' parts = intercalate "\n" (filter (not . null) parts)

renderShadow :: Shadow -> String
renderShadow (Shadow var earlier later) = "warning: " <> later <> " shadows " <> var <> " from " <> earlier

-- | @:env@: each environment and its patches, oldest first. A local one says
-- where it applies, and whether that is here.
describeEnvironments :: FilePath -> ShellState -> String
describeEnvironments cwd state = case overlays state of
  [] -> "no active environments"
  active -> intercalate "\n" (concatMap describe active)
 where
  here = map overlayName (overlays (inScope cwd state))
  flagged = shadows (inScope cwd state)
  scopeOf overlay = case Map.lookup (overlayName overlay) (scopes state) of
    Nothing -> ""
    Just dir -> "  (local to " <> dir <> (if overlayName overlay `elem` here then ")" else ", inactive here)")
  describe overlay =
    (overlayName overlay <> scopeOf overlay)
      : [ "  " <> renderPatch patch <> note (overlayName overlay) patch
        | patch <- overlayPatches overlay ]
  note owner (Patch var _) =
    case [earlier | Shadow v earlier later <- flagged, v == var, later == owner] of
      [] -> ""
      earlier -> "  ⚠ shadows " <> unwords earlier

-- | @:env NAME@: the effective value, then the base and each layer that touched
-- it, oldest first.
describeVariable :: Maybe String -> String -> ShellState -> String
describeVariable base name state =
  intercalate "\n" (header : ("  base  " <> maybe "unset" show base) : layers)
 where
  final = Map.lookup name (effective (maybe Map.empty (Map.singleton name) base) state)
  header = name <> maybe " is unset" (\value -> " = " <> show value) final
  layers = ["  " <> layer <> "  " <> renderOp name op | Touch layer op <- touches name state]

-- | Entering a directory with a @flake.nix@. An allowed flake activates its
-- devshell for this tree, once: coming back restores it from its scope, and
-- the hook does not run again. A flake that changed since is dropped and has
-- to be allowed again. A flake that is not allowed is reported once.
arrive :: FilePath -> Session -> IO (Session, [String])
arrive dir session = do
  found <- flakeHash dir
  case found of
    Nothing -> pure (session, [])
    Just hash
      | Map.lookup dir (sessionFlakes session) == Just hash -> pure (session, [])
      | otherwise -> do
          let name = autoName dir
              stale = Map.member dir (sessionFlakes session)
              cleared = session
                { sessionShell = either (const (sessionShell session)) id (unuseOverlay name (sessionShell session))
                , sessionFlakes = Map.delete dir (sessionFlakes session)
                }
              staleMessage = [name <> ": flake.nix or flake.lock changed" | stale]
          allowed <- isAllowed dir hash
          if allowed then activate name hash cleared staleMessage
          else if Set.member (dir, hash) (sessionNotified session) then pure (cleared, staleMessage)
          else pure
            ( cleared {sessionNotified = Set.insert (dir, hash) (sessionNotified cleared)}
            , staleMessage <> ["flake.nix here is not allowed: allow activates it here and on return"] )
 where
  activate name hash current messages = do
    runtime <- newRuntimeWith (sessionRoots current) (sessionShell current)
    result <- Exception.try (resolveDev runtime ".")
    case result of
      Left (StreamError message) -> pure (current, messages <> [message])
      Right overlay -> do
        let named = overlay {overlayName = name}
        pure ( current
                 { sessionShell = useOverlayIn dir named (sessionShell current)
                 , sessionFlakes = Map.insert dir hash (sessionFlakes current)
                 }
             , messages <> [name <> " activated"] )

-- | Check a declaration and add it to the session, with the text it shows.
-- A data type declared again is a new type: the old one is retired first.
declareIn :: Session -> Declaration -> Either InferError (Session, String)
declareIn session written = do
  let ready = case written of
        DeclData name _ _ | Just _ <- lookupData (sessionClasses session) name -> retire name session
        _ -> session
  done <- declare (sessionClasses ready) (sessionTypes ready) (sessionValues ready) written
  pure
    ( ready
        { sessionClasses = declaredClasses done
        , sessionTypes = foldr (uncurry Map.insert) (sessionTypes ready) (declaredTypes done)
        , sessionValues = foldr (uncurry Map.Lazy.insert) (sessionValues ready) (declaredValues done)
        }
    , declaredMessage done )

-- | Retire a data type: give it the next name, @T#1@, @T#2@, that no
-- written type can spell, in every type the session knows. Bindings that
-- have the old type keep it, and its constructors and instances go with it,
-- so they still work together but never with the new type. The dictionaries
-- of its instances move to their new names.
retire :: String -> Session -> Session
retire name session = session
  { sessionClasses = retireData name stale classes
  , sessionTypes = Map.map (\(Forall bound ty) -> Forall bound (renameType name stale ty)) (sessionTypes session)
  , sessionValues = foldr move (sessionValues session) [instClass i | i <- envInstances classes, instHead i == name]
  }
 where
  classes = sessionClasses session
  stale = name <> "#" <> show (1 + length [() | info <- envData classes, retiredName (dataName info) == Just name])
  move cls values = maybe values (\value -> Map.Lazy.insert (instanceName cls stale) value values)
    (Map.Lazy.lookup (instanceName cls name) values)

-- | The result of one line.
data Step
  = -- | The next session, and text to show. No text means show nothing.
    Continue Session String
  | -- | The line failed, and the session is as it was: what went wrong.
    Failed String
  | Quit

lastWord :: String -> String
lastWord text = case words text of
  [] -> ""
  ws -> last ws

-- | What a line that threw shows, or nothing for an exception that is not
-- the line's to handle, such as Ctrl-C or a killed thread. A language error
-- (division by zero) shows as itself. An @error@ call is a bug in the
-- checker or the evaluator: the line fails, and the shell goes on.
lineFailure :: SomeException -> Maybe String
lineFailure err
  | Just (StreamError message) <- fromException err = Just message
  | Just arith <- fromException err = Just (show (arith :: ArithException))
  | Just (ErrorCall message) <- fromException err = Just ("internal error: " <> message)
  | Just (PatternMatchFail message) <- fromException err = Just ("internal error: " <> message)
  | otherwise = Nothing

-- | Force the text a step shows, so that an error inside it fails the line
-- here and not later, when the loop prints it.
settle :: Step -> IO Step
settle result = case result of
  Continue _ output -> result <$ Exception.evaluate (length output)
  Failed output -> result <$ Exception.evaluate (length output)
  Quit -> pure result

-- | Run one line. A line that fails leaves the session as it was. Only a
-- computation at the prompt, or on the right of @<-@, has any effect.
--
-- A unit result shows nothing, so that @print 1@ shows only @1@.
--
-- A line that changes directory can activate or deactivate local overlays,
-- and that is reported after its output.
step :: Session -> String -> IO Step
step session input = do
  before <- getCurrentDirectory
  result <- (stepLine session input >>= settle) `catch` \err -> maybe (throwIO err) (pure . Failed) (lineFailure err)
  after <- getCurrentDirectory
  case result of
    Continue next output | before /= after -> do
      (arrived, messages) <- arrive after next
      -- Only overlays that were local before the line are reported here: a
      -- new one reports itself, and a stale one is dropped, not restored.
      let was = map overlayName (overlays (inScope before (sessionShell session)))
          now = map overlayName (overlays (inScope after (sessionShell arrived)))
          local = Map.keys (scopes (sessionShell session))
          off = [name <> " deactivated" | name <- was, name `notElem` now, name `elem` local]
          on = [name <> " restored" | name <- now, name `notElem` was, name `elem` local]
      pure (Continue arrived (intercalate "\n" (filter (not . null) (output : off <> on <> messages))))
    other -> pure other

stepLine :: Session -> String -> IO Step
stepLine session input =
  case parseCommandWith (sessionFixities session) (sessionBound session) input of
    Left err -> pure (Failed err)
    Right Blank -> pure (Continue session "")
    Right Exit
      | null (sessionJobs session) || sessionWarned session -> pure Quit
      | otherwise -> pure (Continue session {sessionWarned = True}
          "there are jobs: :q again to quit; stopped ones get SIGHUP, running ones keep running")
    Right Allow -> do
      cwd <- getCurrentDirectory
      found <- flakeHash cwd
      case found of
        Nothing -> pure (Continue session "no flake.nix here")
        Just hash -> do
          Trust.allow cwd hash
          (next, messages) <- arrive cwd session
          pure (Continue next (intercalate "\n" messages))
    Right Deny -> do
      cwd <- getCurrentDirectory
      Trust.deny cwd
      let shell = sessionShell session
          name = autoName cwd
          kept = either (const shell) id (unuseOverlay name shell)
          active = Map.member cwd (sessionFlakes session)
      pure (Continue session {sessionShell = if active then kept else shell, sessionFlakes = Map.delete cwd (sessionFlakes session)}
        (if active then name <> " deactivated" else ""))
    Right ListJobs -> do
      states <- mapM (atomically . jobState . entryJob) (sessionJobs session)
      pure (Continue session (intercalate "\n" (zipWith describeEntry states (sessionJobs session))))
    Right (Foreground wanted) ->
      let jobs = sessionJobs session
          chosen = case wanted of
            Nothing -> reverse jobs
            Just n -> filter ((== n) . entryNumber) jobs
      in case chosen of
        [] -> pure (Continue session (maybe "no jobs" (\n -> "no job " <> show n) wanted))
        entry : _ -> do
          let number = entryNumber entry
              rest = session {sessionJobs = filter ((/= number) . entryNumber) jobs}
          continueJob True (entryJob entry)
          awaitEntry rest {sessionWarned = False} entry
    Right (Background wanted) -> do
      states <- mapM (atomically . jobState . entryJob) (sessionJobs session)
      let jobs = sessionJobs session
          stopped = [entry | (entry, Stopped) <- zip jobs states]
          chosen = case wanted of
            Nothing -> reverse stopped
            Just n -> filter ((== n) . entryNumber) stopped
      case chosen of
        [] -> pure (Continue session (maybe "no stopped jobs" (\n -> "no stopped job " <> show n) wanted))
        entry : _ -> do
          let job = entryJob entry
              number = entryNumber entry
              running = entry {entrySeen = Running}
              others = filter ((/= number) . entryNumber) jobs
              shown = jobLine job <> if lastWord (jobLine job) == "&" then "" else " &"
          continueJob False job
          pure (Continue session {sessionJobs = others <> [running]} ("[" <> show number <> "] " <> shown))
    Right (SetFixity fixity names) ->
      pure (Continue session {sessionFixities = foldr (`Map.insert` fixity) (sessionFixities session) names} "")
    Right (Help Nothing) -> pure (Continue session Help.overview)
    Right (Help (Just word)) -> pure (Continue session (helpFor session word))
    Right (Find context written) -> case writtenCheck classes context written of
      Just problem -> pure (Failed problem)
      Nothing -> pure (Continue session (foundMessage session (findByType classes types (context, written))))
    Right (Signature name context written) -> case writtenCheck classes context written of
      Just problem -> pure (Failed problem)
      Nothing -> pure (Continue session {sessionSignatures = Map.insert name (context, written) (sessionSignatures session)} "")
    Right (Declare written) -> pure (either (Failed . renderError) (uncurry Continue) (declareIn session written))
    Right (ShowElabDeclaration written) -> case declare classes types values written of
      Left err -> pure (Failed (renderError err))
      Right done -> pure (Continue session (maybe (declaredMessage done) (\(name, code) -> "(" <> name <> ") = " <> renderCore code) (declaredCode done)))
    Right (ShowCore expression) -> pure (Continue session (renderCore expression))
    Right (Import alias path) -> do
      loaded <- loadModule session path
      pure $ case loaded of
        Left message -> Failed message
        Right found ->
          let (next, added) = mergeModule alias found session
              shown = case filter ((/= "%") . take 1) added of
                [] -> "imported nothing"
                visible -> "imported " <> intercalate ", " (map displayName visible)
          in Continue next (intercalate "\n" (shown : moduleWarnings found))
    Right (ShowElab named expression) ->
      let mode = maybe Defaulting (const Generalising) named
      in checked (elaborateWith classes mode types expression) $ \(_, code) ->
        pure (Continue session (maybe "" ((<> " = ") . displayName) named <> renderCore code))
    Right (TypeOf expression) -> checked (inferWith classes types expression) $ \ty ->
      pure (Continue session (renderEffectsType ty))
    Right (ShowEnv Nothing) -> do
      cwd <- getCurrentDirectory
      pure (Continue session (describeEnvironments cwd (sessionShell session)))
    Right (ShowEnv (Just name)) -> do
      base <- lookupEnv name
      cwd <- getCurrentDirectory
      pure (Continue session (describeVariable base name (inScope cwd (sessionShell session))))
    -- What runs is the elaborated line: each class method gets its
    -- dictionary. A line that runs defaults its ambiguous unknowns to ().
    Right (Run expression) -> checked (elaborateWith classes Defaulting types expression) $ \(ty, code) ->
      case snd (unclass (snd (unqualify ty))) of
        TComp _ resultTy -> preflight ty $ \runtime ->
          foreground session input runtime (eval values code) $ \done now result ->
            committed done start now (showResult (Just resultTy) result)
        TStream _ _ _ _ -> checked (inferRunWith classes types expression) $ \_ ->
          preflight ty $ \runtime ->
            foreground session input runtime (eval values code) $ \done now result ->
              committed done start now (showResult Nothing result)
        valueTy -> pure (Continue session (showValue session valueTy (eval values code)))
    -- A binding abstracts over the dictionaries its type needs.
    -- A signature, if one waits, decides the binding's type; it is used up.
    Right (Bind name expression) ->
      let signed = Map.lookup name (sessionSignatures session)
          elaborated = maybe (elaborateWith classes Generalising types expression) (\sig -> elaborateSigned classes types sig expression) signed
          used now = now {sessionSignatures = Map.delete name (sessionSignatures now)}
      in checked elaborated $ \(ty, code) ->
        pure (bound (used session) name ty (eval values code))
    -- Each member is the group with that member as its body, so each gets
    -- the group's generalisation, and a waiting signature as Bind does. The
    -- group binds only if every member checks.
    Right (BindRec group) ->
      let members = [(name, ELetRec group (EVar name)) | (name, _) <- group]
          elaborate (name, expression) = (,) name <$> maybe
            (elaborateWith classes Generalising types expression)
            (\sig -> elaborateSigned classes types sig expression)
            (Map.lookup name (sessionSignatures session))
      in checked (mapM elaborate members) $ \done ->
        let add (now, shown) (name, (ty, code)) = case bound now {sessionSignatures = Map.delete name (sessionSignatures now)} name ty (eval values code) of
              Continue next text -> (next, shown <> [text])
              _ -> (now, shown)
            (final, texts) = foldl add (session, []) done
        in pure (Continue final (intercalate "\n" texts))
    Right (BindRun name expression) -> checked (elaborateRunWith classes types expression) $ \(actionTy, ty, code) ->
      preflight actionTy $ \runtime ->
        -- A job stopped here binds when fg finishes it, in the session then.
        foreground session input runtime (eval values code) $ \done now value ->
          case bound now name ty value of
            Continue next text -> committed done start next text
            other -> pure other
  where
    types = sessionTypes session
    values = sessionValues session
    start = sessionShell session
    checked result onSuccess = case result of
      Left err -> pure (Failed (renderError err))
      Right ty -> onSuccess ty
    -- Generalising after a run is sound while no value can be mutable. A
    -- mutable reference would need a value restriction here.
    bound now name ty value =
      let next = now
            { sessionTypes = Map.insert name (generalise (sessionTypes now) ty) (sessionTypes now)
            , sessionValues = Map.insert name value (sessionValues now)
            }
      in Continue next (displayName name <> " : " <> renderType ty)
    -- Commands resolve in the effective environment, so an overlay that
    -- prepends to PATH changes what the preflight finds.
    preflight ty action = do
      let (constraints, _) = unqualify ty
      case closeEffects constraints (actionEffects ty) of
        Nothing -> pure (Failed "cannot run an action with an unresolved effect row")
        Just (EffRow labels _) -> do
          runtime <- newRuntimeWith (sessionRoots session) (sessionShell session)
          env <- runtimeEnviron runtime
          missing <- filterM (fmap not . programExists env) [name | Exec name <- Set.toList labels]
          case missing of
            [] -> action runtime
            name : _ -> pure (Failed ("command not found: " <> name))
    classes = sessionClasses session
    showResult _ VUnit = ""
    showResult _ (VStatus (ExitedWith 0)) = ""
    showResult _ (VJob job _) = "[" <> show (jobNumber job) <> "] " <> jobLine job
    showResult (Just resultTy) value = showValue session resultTy value
    showResult Nothing value = render value

-- | A name as the prompt shows it: an operator in parentheses.
displayName :: Name -> String
displayName name
  | isOperator name = "(" <> name <> ")"
  | otherwise = name

-- | A value as the prompt shows it: through Show, where its type has an
-- instance, so that a declared instance changes what the prompt prints.
-- Other values, such as functions, show as before.
showValue :: Session -> Ty -> Value -> String
showValue session ty value =
  case elaborateWith (sessionClasses session) Defaulting (Map.insert it (mono ty) (sessionTypes session)) (EApp (EVar "%show") (EVar it)) of
    Right (_, code) -> case eval (Map.insert it value (sessionValues session)) code of
      VStr text -> text
      _ -> render value
    Left _ -> render value
 where
  it = "%it"

-- | Where the startup file is: @$XDG_CONFIG_HOME/lambdash/init.lsh@, or
-- @~/.config/lambdash/init.lsh@.
startupFile :: IO FilePath
startupFile = (<> "/init.lsh") <$> getXdgDirectory XdgConfig "lambdash"

-- | Run a file's lines as if typed, in order: bindings, fixity lines,
-- declarations and commands. A @do@ block may span lines, as at the prompt.
-- A line that fails is reported with its line number and the rest still
-- run; @:q@ stops the file, not the shell. Bindings, declarations and
-- imports say nothing but their warnings, so only output and errors show.
-- An import starts from the file's directory. A missing file is no file.
runScript :: FilePath -> Session -> IO (Session, [String])
runScript path session = do
  exists <- doesFileExist path
  if not exists
    then pure (session, [])
    else do
      text <- readFile path
      _ <- Exception.evaluate (length text)
      directory <- takeDirectory <$> makeAbsolute path
      (done, reports) <- go session {sessionDirectory = Just directory} (scriptChunks text)
      pure (done {sessionDirectory = sessionDirectory session}, reports)
 where
  go now [] = pure (now, [])
  go now ((n, chunk) : rest) = do
    result <- step now chunk
    case result of
      Quit -> pure (now, [])
      Failed message -> fmap ((path <> ":" <> show n <> ": " <> message) :) <$> go now rest
      Continue next output -> do
        let shown
              | quiet next chunk = filter ("warning:" `isPrefixOf`) (lines output)
              | otherwise = [output | not (null output)]
        fmap (shown <>) <$> go next rest
  quiet now chunk = case parseCommandWith (sessionFixities now) AllBound chunk of
    Right (Bind _ _) -> True
    Right (BindRec _) -> True
    Right (BindRun _ _) -> True
    Right (Declare _) -> True
    Right (Signature {}) -> True
    Right (Import _ _) -> True
    _ -> False

-- | A file's lines, numbered, with a @do@ block or @case@ that spans lines
-- joined into one chunk, as the prompt joins them.
scriptChunks :: String -> [(Int, String)]
scriptChunks = chunks . zip [1 ..] . lines
 where
  chunks [] = []
  chunks ((n, line) : rest) = gather n line rest
  gather n acc rest
    | needsMore acc, (_, next) : more <- rest = gather n (acc <> "\n" <> next) more
    | otherwise = (n, acc) : chunks rest

-- | What a session's lines are parsed against: its names, and the builtins
-- that are still the builtin, whose signatures guide parsing.
sessionBound :: Session -> Bound
sessionBound session = BoundWith (Map.keysSet types) builtinNames
 where
  types = sessionTypes session
  builtinNames = Map.keysSet (Map.filterWithKey (\name scheme -> Map.lookup name typeEnv == Just scheme) types)

-- | A loaded module: the session that its lines built, the names its
-- bindings made, and the names its declarations made. An import may put
-- the first under a prefix. The second always go in as they are, because
-- data types, classes and instances are global.
data Module = Module
  { moduleSession :: Session
  , moduleNames :: [Name]
  , moduleGlobals :: [Name]
  , moduleWarnings :: [String]
  }

-- | Load a module. It starts from the prelude, not from the importer, and
-- holds only definitions: a line that would run something stops the load,
-- so an import never runs code. A relative path starts at the importing
-- file's directory.
loadModule :: Session -> FilePath -> IO (Either String Module)
loadModule session written = do
  base <- maybe getCurrentDirectory pure (sessionDirectory session)
  home <- getHomeDirectory
  let path = case written of
        '~' : '/' : rest -> home </> rest
        _ -> base </> written
  exists <- doesFileExist path
  if not exists
    then pure (Left ("no module at " <> written))
    else do
      canonical <- canonicalizePath path
      if canonical `elem` sessionImporting session
        then pure (Left ("an import cycle: " <> intercalate " -> " (reverse (canonical : sessionImporting session))))
        else do
          text <- readFile canonical
          _ <- Exception.evaluate (length text)
          let start = initial
                { sessionRoots = sessionRoots session
                , sessionDirectory = Just (takeDirectory canonical)
                , sessionImporting = canonical : sessionImporting session
                }
          go canonical (Module start [] [] []) (scriptChunks text)
 where
  go _ found [] = pure (Right found)
  go path found ((n, chunk) : rest) = do
    let now = moduleSession found
        at message = pure (Left (path <> ":" <> show n <> ": " <> message))
    case parseCommandWith (sessionFixities now) (sessionBound now) chunk of
      Left err -> at err
      -- A module's own imports are not exported, but their declarations
      -- are global, so they come along.
      Right (Import alias inner) -> do
        loaded <- loadModule now inner
        case loaded of
          Left message -> at message
          Right nested ->
            let (next, _) = mergeModule alias nested now
            in checked path n found
                 { moduleSession = next
                 , moduleGlobals = moduleGlobals found <> moduleGlobals nested
                 , moduleWarnings = moduleWarnings found <> moduleWarnings nested
                 } rest
      Right line -> case defines now line of
        Nothing -> at "a module holds only definitions, and this line would run"
        Just (names, globals) -> do
          result <- stepLine now chunk
          case result of
            Continue next output -> checked path n found
              { moduleSession = next
              , moduleNames = moduleNames found <> names
              , moduleGlobals = moduleGlobals found <> globals
              , moduleWarnings = moduleWarnings found <> filter ("warning:" `isPrefixOf`) (lines output)
              } rest
            Failed message -> at message
            Quit -> at "a module cannot quit"
  -- A module that declares a data type twice would carry a retired type,
  -- @T#1@, whose name can clash with the importer's own.
  checked path n found rest = case [dataName info | info <- envData (sessionClasses (moduleSession found)), Just _ <- [retiredName (dataName info)]] of
    retired : _ -> pure (Left (path <> ":" <> show n <> ": the module declares the data type " <> maybe retired id (retiredName retired) <> " twice"))
    [] -> go path found rest

-- | The names a definition makes: those that a binding makes, which an
-- import may qualify, and those that a declaration makes, which it does
-- not. An operator cannot be written with a prefix, so it goes in as it
-- is. Nothing for a line that would run, or that is not a definition.
defines :: Session -> Command -> Maybe ([Name], [Name])
defines session line = case line of
  Blank -> Just ([], [])
  SetFixity _ _ -> Just ([], [])
  Signature {} -> Just ([], [])
  Bind name _ -> Just (partition (not . isOperator) [name])
  BindRec group -> Just (partition (not . isOperator) (map fst group))
  Declare written -> Just ([], either (const []) made (declare (sessionClasses session) (sessionTypes session) (sessionValues session) written))
  _ -> Nothing
 where
  made done = map fst (declaredTypes done) <> map fst (declaredValues done)

-- | Bring a module's names into a session, with its classes, data types,
-- instances and fixities. Its bindings go under the prefix, if there is
-- one. Gives the names added.
mergeModule :: Maybe Name -> Module -> Session -> (Session, [Name])
mergeModule alias found original =
  ( session
      { sessionTypes = foldl (\types (to, ty, _) -> maybe types (\known -> Map.insert to known types) ty) (sessionTypes session) added
      , sessionValues = foldl (\values (to, _, value) -> Map.Lazy.insert to value values) (sessionValues session) added
      , sessionClasses = mergeClasses (Set.fromList (moduleGlobals found)) (sessionClasses inner) (sessionClasses session)
      , sessionFixities = Map.union (Map.differenceWith same (sessionFixities inner) defaultFixities) (sessionFixities session)
      }
  , [to | (to, _, _) <- added]
  )
 where
  -- The module's data types are new types, so ours of the same name retire.
  session = foldr retire original
    [dataName info | info <- envData (sessionClasses inner), Just _ <- [lookupData (sessionClasses original) (dataName info)]]
  inner = moduleSession found
  prefixed name = maybe name (\prefix -> prefix <> "." <> name) alias
  pairs = [(prefixed name, name) | name <- nub (moduleNames found)] <> [(name, name) | name <- nub (moduleGlobals found)]
  -- The lookup leaves the value itself unevaluated. An instance's
  -- dictionary has a value and no type.
  added = [(to, Map.lookup from (sessionTypes inner), value) | (to, from) <- pairs, Just value <- [Map.Lazy.lookup from (sessionValues inner)]]
  same theirs builtin = if theirs == builtin then Nothing else Just theirs

-- | Add a module's classes, data types and instances to a session's. The
-- module started from the prelude, so what is not the prelude's is its own.
-- Instances go last, because a class or data type declared again drops the
-- instances it had.
--
-- An instance comes with its dictionary: the module's if it declared one,
-- which may replace a prelude instance, and otherwise neither. The checker
-- passes a dictionary the arguments its instance says it requires, so the
-- two must not come from different places.
--
-- This cannot make a superclass cycle. The module's classes have
-- superclasses only in the module or the prelude, and they replace ours of
-- the same name, so no path leads from them back to ours.
mergeClasses :: Set.Set Name -> ClassEnv -> ClassEnv -> ClassEnv
mergeClasses dictionaries theirs ours = foldl (flip addInstance) withData newInstances
 where
  withClasses = foldl (flip addClass) ours [info | info <- envClasses theirs, className info `notElem` map className (envClasses prelude)]
  withData = foldl (flip addData) withClasses (envData theirs)
  newInstances = [inst | inst <- envInstances theirs, Set.member (instanceName (instClass inst) (instHead inst)) dictionaries]

-- | Whether a name is an operator, such as @<+>@.
isOperator :: Name -> Bool
isOperator name = not (null name) && all (`elem` ("!%&*+-/<=>?@^|~:" :: String)) name

-- | Whether a signature can stand: its classes exist, and its context
-- constrains only its own unknowns.
writtenCheck :: ClassEnv -> [SPred] -> SType -> Maybe String
writtenCheck classes context written =
  case [c | SPred c _ <- context, null (lookupClass classes c)] of
    c : _ -> Just ("no class " <> c)
    [] | not (all onOwn context) -> Just "a signature's context constrains its own unknowns"
       | otherwise -> Nothing
 where
  onOwn (SPred _ (STVar v)) = v `elem` names written
  -- A class on a record's tail, as in Show { | r} => {name : String | r} -> String.
  onOwn (SPred _ (STRecord [] (Just v))) = v `elem` names written
  onOwn _ = False
  names t = case t of
    STVar v -> [v]
    STList a -> names a
    STFun a b -> names a <> names b
    STRecord fields rest -> concatMap (names . snd) fields <> maybe [] pure rest
    STApp _ _ args -> concatMap names args
    STCon _ -> []

-- | Help on one word: a topic, a class, or a name in scope, with its type
-- as the session has it now.
helpFor :: Session -> String -> String
helpFor session word
  | Just text <- Help.topic word = text
  | Just info <- lookupClass classes word = classHelp classes info
  | Just info <- lookupData classes word = dataHelp info
  | Just scheme <- Map.lookup word types =
      let ty = either (const "?") renderEffectsType (inferWith classes types (EVar word))
          builtin = Map.lookup word typeEnv == Just scheme
          described = if builtin then maybe [] pure (Help.describe word) else []
          fixity = case Map.lookup word (sessionFixities session) of
            Just (Fixity assoc level) -> [assocWord assoc <> " " <> show level]
            Nothing | operator -> ["infixl 9"]
                    | otherwise -> []
      in intercalate "\n" ((displayName word <> " : " <> ty) : map ("  " <>) (described <> fixity))
  | otherwise = "nothing named " <> word <> "; :help lists the topics"
 where
  classes = sessionClasses session
  types = sessionTypes session
  operator = not (null word) && all (`elem` ("!%&*+-/<=>?@^|~:" :: String)) word
  assocWord InfixL = "infixl"
  assocWord InfixR = "infixr"
  assocWord InfixN = "infix"

-- | A data type, as declared.
-- | For each retired type that a text names, what it was: an error that
-- says only @T#1 and T@ would not say why they differ.
retiredNote :: String -> String
retiredNote text = concat ["\n  " <> name <> " is an earlier declaration of " <> base | name <- nub (words (map keep text)), Just base <- [retiredName name]]
 where
  keep c = if isAlphaNum c || c `elem` ("#_'" :: String) then c else ' '

dataHelp :: DataInfo -> String
dataHelp info = "data " <> unwords (dataName info : params) <> " = " <> intercalate " | " (map constructor (dataConstructors info))
  <> retiredNote (dataName info)
 where
  names = Map.fromList (zip (dataParams info) params)
  params = take (length (dataParams info)) [[c] | c <- ['a' ..]]
  constructor (c, fields) = unwords (c : map field fields)
  field ty = let text = evalState (renderTy ty) names in case ty of
    TData _ (_ : _) -> "(" <> text <> ")"
    TFun _ _ -> "(" <> text <> ")"
    _ -> text

-- | A class: its superclasses, methods and instances.
classHelp :: ClassEnv -> ClassInfo -> String
classHelp classes info = intercalate "\n" (header : methods <> [instancesLine])
 where
  name = className info
  supers = case classSupers info of
    [] -> ""
    [one] -> one <> " a => "
    several -> "(" <> intercalate ", " [c <> " a" | c <- several] <> ") => "
  header = "class " <> supers <> name <> " a"
  -- The class's unknown is always a.
  method (m, ty) = "  " <> displayName m <> " : " <> evalState (renderVar (classVar info) >> renderTy ty) Map.empty
    <> if m `elem` map fst (classDefaults info) then "   (default)" else ""
  methods = map method (classMethods info)
  instancesLine = case [inst | inst <- envInstances classes, instClass inst == name] of
    [] -> "no instances"
    found -> "instances  " <> intercalate ", " (map instanceText found)
  instanceText inst =
    let letters = ["a", "b", "c", "d"]
        args = zip letters (instRequires inst)
        shape = case instHead inst of
          "[]" -> "[a]"
          "->" -> "(a -> b)"
          "{}" -> "records"
          other | null (instRequires inst) -> other
                | otherwise -> other <> " " <> unwords (take (length (instRequires inst)) letters)
        context = [c <> " " <> v | (v, cs) <- args, c <- cs]
    in shape <> if null context then "" else " when " <> intercalate ", " context

-- | What @:find@ shows: each name with its type, as @:t@ shows it, best
-- first. A name whose arguments had to change order says so. Twenty at
-- most, with a count of the rest.
foundMessage :: Session -> [Found] -> String
foundMessage _ [] = "nothing has that type"
foundMessage session found = intercalate "\n" (map line shown <> more)
 where
  (shown, rest) = splitAt 20 found
  more = [ "… and " <> show (length rest) <> " more" | not (null rest)]
  line (Found name reordered) =
    displayName name <> " : " <> typeOf name <> (if reordered then "   (arguments in another order)" else "")
  typeOf name = either (const "?") renderEffectsType
    (inferWith (sessionClasses session) (sessionTypes session) (EVar name))
