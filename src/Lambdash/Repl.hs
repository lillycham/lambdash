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

import Control.Monad (filterM)
import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import Data.Bifunctor (first)
import Data.Char (chr, ord)
import Data.Map.Strict qualified as Map
import Lambdash.Eval
import Lambdash.Infer
import Lambdash.Parser
import Lambdash.Process (Status (..), programExists)
import Lambdash.Unify
import Text.Megaparsec ((<|>), eof, errorBundlePretty, try)

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
  | -- | @x <- expr@.
    BindRun Name Expr
  | -- | @expr@.
    Run Expr
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
  <|> (Bind <$> try (lexeme identifier <* symbol "=") <*> expr)
  <|> (BindRun <$> try (lexeme identifier <* symbol "<-") <*> expr)
  <|> (Run <$> expr)

-- | Parse one whole line, or give a readable error.
parseCommand :: Bound -> String -> Either String Command
parseCommand bound = first errorBundlePretty . runIn bound (sc *> command <* eof)

-- | Show a type. Unknowns get letters in first-seen order, so the numbers from
-- the checker never reach the user: @a -> a@, @(a -> b) -> a -> b@,
-- @{size : a | b} -> a@, @{a : Int, b : Bool}@, @{}@.
--
-- Note: an unknown can be in a row tail as well as in a type, and they share
-- letters. Only the left of an arrow needs parentheses.
renderType :: Ty -> String
renderType ty = evalState (renderTy ty) Map.empty

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
renderTy (TFun argument result) = do
  left <- renderTy argument
  right <- renderTy result
  let parameter = case argument of
        TFun _ _ -> "(" <> left <> ")"
        _ -> left
  pure (parameter <> " -> " <> right)
renderTy (TComp result) = do
  text <- renderTy result
  pure $ case result of
    TFun _ _ -> "Comp (" <> text <> ")"
    TComp _ -> "Comp (" <> text <> ")"
    _ -> "Comp " <> text
renderTy (TRecord row) = do
  fields <- renderRow row
  pure ("{" <> fields <> "}")

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

renderPair :: String -> Ty -> Ty -> String
renderPair message left right = flip evalState Map.empty $ do
  leftText <- renderTy left
  rightText <- renderTy right
  pure (message <> leftText <> " and " <> rightText)

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
step session input =
  case parseCommand (BoundNames (Map.keysSet types)) input of
    Left err -> pure (Continue session err)
    Right Blank -> pure (Continue session "")
    Right Exit -> pure Quit
    Right (TypeOf expression) -> checked (inferIn types expression) $ \ty ->
      pure (Continue session (renderType ty))
    Right (Run expression) -> checked (inferIn types expression) $ \ty ->
      case ty of
        TComp _ -> preflight expression $ do
          result <- runComp (eval values expression)
          pure (Continue session (showResult result))
        _ -> pure (Continue session (render (eval values expression)))
    Right (Bind name expression) -> checked (inferIn types expression) $ \ty ->
      pure (bound name ty (eval values expression))
    Right (BindRun name expression) -> checked (inferRunIn types expression) $ \ty ->
      preflight expression (bound name ty <$> runComp (eval values expression))
  where
    types = sessionTypes session
    values = sessionValues session
    checked result onSuccess = case result of
      Left err -> pure (Continue session (renderError err))
      Right ty -> onSuccess ty
    -- Generalising after a run is sound while no value can be mutable. A
    -- mutable reference would need a value restriction here.
    bound name ty value =
      let next = Session
            { sessionTypes = Map.insert name (generalise types ty) types
            , sessionValues = Map.insert name value values
            }
      in Continue next (name <> " : " <> renderType ty)
    -- This sees only the commands that the line names. The commands inside a
    -- bound name wait for effect rows.
    preflight expression action = do
      missing <- filterM (fmap not . programExists) (commands expression)
      case missing of
        [] -> action
        name : _ -> pure (Continue session ("command not found: " <> name))
    showResult VUnit = ""
    showResult (VStatus (ExitedWith 0)) = ""
    showResult value = render value
