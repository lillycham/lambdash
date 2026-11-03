-- | The REPL as a pure function: a session and a line give a new session and
-- some text. The loop in @app/Main.hs@ only moves text, so everything here can
-- be tested without a terminal.
module Lambdash.Repl
  ( Session (..)
  , initial
  , sessionPrompt
  , needsMore
  , Command (..)
  , command
  , parseCommand
  , renderType
  , renderEffectsType
  , renderError
  , Step (..)
  , step
  , Job
  , jobNumber
  , jobLine
  ) where

import Control.Monad (filterM)
import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import Data.Bifunctor (first)
import Data.Char (chr, ord)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.List (intercalate)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (SomeException, catch, fromException, throwIO)
import Control.Exception qualified as Exception
import Data.Char (isDigit)
import Lambdash.Stream (StreamError (..))
import Lambdash.Eval
import Lambdash.Infer
import Lambdash.Parser
import Data.IORef (readIORef)
import Lambdash.Overlay
import Lambdash.Runtime
import Lambdash.Process (Status (..), programExists)
import Lambdash.Unify
import Text.Megaparsec ((<|>), eof, errorBundlePretty, many, satisfy, try)
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
    sessionJobs :: [Job]
  , -- | Whether @:q@ has already warned about stopped jobs.
    sessionWarned :: Bool
  }

-- | A line that Ctrl-Z stopped. It is still running, parked on its own
-- thread, and holds its processes, pipes and workers as they were.
data Job = Job
  { jobNumber :: Int
  , jobLine :: String
  , jobEvents :: MVar Event
  , jobResume :: MVar ()
  , -- | What the line does with its result, given the session at that time.
    jobFinish :: Session -> Value -> IO Step
  }

data Event = Finished (Either SomeException Value) | Suspended

-- | The session before any input.
initial :: Session
initial = Session typeEnv scope emptyState Nothing [] False

-- | Does this input open more @do@ blocks than it closes? The REPL then reads
-- another line. Only whole words count, outside string literals.
needsMore :: String -> Bool
needsMore input = depth (tokens (withoutStrings input)) > 0
 where
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
    "end" -> n - 1
    _ -> n) (0 :: Int)

-- | What the prompt should say: the active environments, and any shadowing.
sessionPrompt :: Session -> String
sessionPrompt = prompt . sessionShell

-- | What one line asks for.
data Command
  = -- | Nothing, or only a comment.
    Blank
  | -- | @:q@.
    Exit
  | -- | @:t expr@.
    TypeOf Expr
  | -- | @:env@ lists the active environments and their patches. @:env NAME@
    -- shows a variable, and each layer that touched it.
    ShowEnv (Maybe String)
  | -- | @x = expr@.
    Bind Name Expr
  | -- | @x <- expr@.
    BindRun Name Expr
  | -- | @expr@.
    Run Expr
  | -- | @jobs@, when nothing binds it.
    ListJobs
  | -- | @fg@, or @fg 2@, when nothing binds it.
    Foreground (Maybe Int)
  deriving (Eq, Show)

-- | One line.
--
-- Note: @x = 1@ starts like the expression @x@ until the @=@ arrives. Reserved
-- words help too: @keyword ":t"@ works here.
command :: Parser Command
command =
      (Blank <$ eof)
  <|> (Exit <$ lexeme (keyword ":q"))
  <|> (TypeOf <$> (lexeme (keyword ":t") *> expr))
  <|> (ShowEnv <$> (lexeme (keyword ":env") *> optionalName))
  <|> (Bind <$> try (lexeme identifier <* symbol "=") <*> expr)
  <|> (BindRun <$> try (lexeme identifier <* symbol "<-") <*> expr)
  <|> (Run <$> expr)

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
parseCommand bound input = case words input of
  ["jobs"] | free "jobs" -> Right ListJobs
  ["fg"] | free "fg" -> Right (Foreground Nothing)
  ["fg", number] | free "fg", Just n <- jobNumberOf number -> Right (Foreground (Just n))
  _ -> first errorBundlePretty (runIn bound (sc *> command <* eof) input)
 where
  free name = case bound of
    AllBound -> False
    BoundNames names -> Set.notMember name names
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
hideEffects (TList ty) = TList (hideEffects ty)
hideEffects (TStage i o) = TStage (hideEffects i) (hideEffects o)
hideEffects (TCollector i o) = TCollector (hideEffects i) (hideEffects o)
hideEffects (TComp _ t) = TComp noEffects (hideEffects t)
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
renderTy (TStage i o) = renderAdapter "Stage" i o
renderTy (TCollector i o) = renderAdapter "Collector" i o
renderTy (TQualified constraints ty) = do
  conditions <- mapM renderConstraint constraints
  text <- renderTy ty
  pure ("(" <> intercalate ", " conditions <> ") => " <> text)
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
    TStream _ _ _ _ -> "Comp" <> effects <> " (" <> text <> ")"
    TStage _ _ -> "Comp" <> effects <> " (" <> text <> ")"
    TCollector _ _ -> "Comp" <> effects <> " (" <> text <> ")"
    _ -> "Comp" <> effects <> " " <> text
renderTy (TStream eff input output result) = do
  effects <- renderEffects eff
  parts <- mapM renderTy [input, output, result]
  let argument ty text = case ty of
        TFun _ _ -> "(" <> text <> ")"
        TComp _ _ -> "(" <> text <> ")"
        TStream _ _ _ _ -> "(" <> text <> ")"
        TStage _ _ -> "(" <> text <> ")"
        TCollector _ _ -> "(" <> text <> ")"
        _ -> text
  pure ("Stream" <> effects <> " " <> unwords (zipWith argument [input, output, result] parts))
renderTy (TRecord row) = do
  fields <- renderRow row
  pure ("{" <> fields <> "}")

renderAdapter :: String -> Ty -> Ty -> State TypeNames String
renderAdapter name input output = do
  parts <- mapM renderTy [input, output]
  let argument ty text = case ty of
        TFun _ _ -> "(" <> text <> ")"
        TComp _ _ -> "(" <> text <> ")"
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
renderError (Unify err) = case err of
  Mismatch left right -> renderPair "type mismatch: " left right
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
  let before = shadows (sessionShell next)
      warnings = [renderShadow shadow | shadow <- shadows state, shadow `notElem` before]
      output extra = intercalate "\n" (filter (not . null) (extra <> [text]))
  pure $ if state == start then Continue next (output [])
    else if sessionShell next /= start
      then Continue next (output ["warning: the environments changed while this job was stopped, so its changes to them are dropped"])
      else Continue next {sessionShell = state} (output warnings)

-- | Run a line's computation on its own thread, so that Ctrl-Z can park it.
-- Then wait for it, as 'awaitJob' does.
foreground :: Session -> String -> Runtime -> Value -> (Runtime -> Session -> Value -> IO Step) -> IO Step
foreground session line runtime0 value finish = do
  events <- newEmptyMVar
  resume <- newEmptyMVar
  let runtime = runtime0 {runtimePark = Just (putMVar events Suspended >> takeMVar resume)}
      number = 1 + maximum (0 : map jobNumber (sessionJobs session))
  _ <- forkIO (Exception.try (runComp runtime value) >>= putMVar events . Finished)
  awaitJob session (Job number line events resume (finish runtime))

-- | Wait until a job ends or stops. A stopped job joins the table.
awaitJob :: Session -> Job -> IO Step
awaitJob session job = do
  event <- takeMVar (jobEvents job)
  case event of
    Finished (Right value) -> jobFinish job session value
    Finished (Left err) -> case fromException err of
      Just (StreamError message) -> pure (Continue session message)
      Nothing -> throwIO err
    Suspended ->
      pure (Continue session {sessionJobs = sessionJobs session <> [job], sessionWarned = False}
        (describeJob job))

describeJob :: Job -> String
describeJob job = "[" <> show (jobNumber job) <> "] stopped  " <> jobLine job

renderShadow :: Shadow -> String
renderShadow (Shadow var earlier later) = "warning: " <> later <> " shadows " <> var <> " from " <> earlier

-- | @:env@: each active environment and its patches, oldest first.
describeEnvironments :: ShellState -> String
describeEnvironments state = case overlays state of
  [] -> "no active environments"
  active -> intercalate "\n" (concatMap describe active)
 where
  flagged = shadows state
  describe overlay =
    overlayName overlay
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

-- | The result of one line.
data Step
  = -- | The next session, and text to show. No text means show nothing.
    Continue Session String
  | Quit

-- | Run one line. A line that fails leaves the session as it was. Only a
-- computation at the prompt, or on the right of @<-@, has any effect.
--
-- A unit result shows nothing, so that @print 1@ shows only @1@.
step :: Session -> String -> IO Step
step session input = stepLine session input `catch` \(StreamError message) -> pure (Continue session message)

stepLine :: Session -> String -> IO Step
stepLine session input =
  case parseCommand (BoundNames (Map.keysSet types)) input of
    Left err -> pure (Continue session err)
    Right Blank -> pure (Continue session "")
    Right Exit
      | null (sessionJobs session) || sessionWarned session -> pure Quit
      | otherwise -> pure (Continue session {sessionWarned = True}
          "there are stopped jobs: :q again to quit, and they get SIGHUP")
    Right ListJobs -> pure (Continue session (intercalate "\n" (map describeJob (sessionJobs session))))
    Right (Foreground wanted) ->
      let jobs = sessionJobs session
          chosen = case wanted of
            Nothing -> reverse jobs
            Just n -> filter ((== n) . jobNumber) jobs
      in case chosen of
        [] -> pure (Continue session (maybe "no stopped jobs" (\n -> "no job " <> show n) wanted))
        job : _ -> do
          let rest = session {sessionJobs = filter ((/= jobNumber job) . jobNumber) jobs}
          putMVar (jobResume job) ()
          awaitJob rest job
    Right (TypeOf expression) -> checked (inferIn types expression) $ \ty ->
      pure (Continue session (renderEffectsType ty))
    Right (ShowEnv Nothing) -> pure (Continue session (describeEnvironments (sessionShell session)))
    Right (ShowEnv (Just name)) -> do
      base <- lookupEnv name
      pure (Continue session (describeVariable base name (sessionShell session)))
    Right (Run expression) -> checked (inferIn types expression) $ \ty ->
      case snd (unqualify ty) of
        TComp _ _ -> preflight ty $ \runtime ->
          foreground session input runtime (eval values expression) $ \done now result ->
            committed done start now (showResult result)
        TStream _ _ _ _ -> checked (inferRunIn types expression) $ \_ ->
          preflight ty $ \runtime ->
            foreground session input runtime (eval values expression) $ \done now result ->
              committed done start now (showResult result)
        _ -> pure (Continue session (render (eval values expression)))
    Right (Bind name expression) -> checked (inferIn types expression) $ \ty ->
      pure (bound session name ty (eval values expression))
    Right (BindRun name expression) -> checked (inferIn types expression) $ \actionTy ->
      checked (inferRunIn types expression) $ \ty ->
        preflight actionTy $ \runtime ->
          -- A job stopped here binds when fg finishes it, in the session then.
          foreground session input runtime (eval values expression) $ \done now value ->
            case bound now name ty value of
              Continue next text -> committed done start next text
              other -> pure other
  where
    types = sessionTypes session
    values = sessionValues session
    start = sessionShell session
    checked result onSuccess = case result of
      Left err -> pure (Continue session (renderError err))
      Right ty -> onSuccess ty
    -- Generalising after a run is sound while no value can be mutable. A
    -- mutable reference would need a value restriction here.
    bound now name ty value =
      let next = now
            { sessionTypes = Map.insert name (generalise (sessionTypes now) ty) (sessionTypes now)
            , sessionValues = Map.insert name value (sessionValues now)
            }
      in Continue next (name <> " : " <> renderType ty)
    -- Commands resolve in the effective environment, so an overlay that
    -- prepends to PATH changes what the preflight finds.
    preflight ty action = do
      let (constraints, _) = unqualify ty
      case closeEffects constraints (actionEffects ty) of
        Nothing -> pure (Continue session "cannot run an action with an unresolved effect row")
        Just (EffRow labels _) -> do
          runtime <- newRuntimeWith (sessionRoots session) (sessionShell session)
          env <- runtimeEnviron runtime
          missing <- filterM (fmap not . programExists env) [name | Exec name <- Set.toList labels]
          case missing of
            [] -> action runtime
            name : _ -> pure (Continue session ("command not found: " <> name))
    showResult VUnit = ""
    showResult (VStatus (ExitedWith 0)) = ""
    showResult value = render value
