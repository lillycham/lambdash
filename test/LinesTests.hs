module Main (main) where

import Control.Exception
import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.IORef
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Lambdash.Lines
import Lambdash.Stream (StreamError (..))
import System.Exit (exitFailure)
import System.Timeout (timeout)

split :: [BS.ByteString] -> IO [String]
split chunks = do
  input <- newIORef chunks
  output <- newIORef []
  let receive = atomicModifyIORef' input (\xs -> case xs of [] -> ([], Nothing); x : rest -> (rest, Just x))
  runLines receive (\line -> modifyIORef' output (line :))
  reverse <$> readIORef output

utf8 :: String -> BS.ByteString
utf8 = Text.encodeUtf8 . Text.pack

-- Every possible partition of a short UTF-8 stream, including splits inside
-- every multibyte character and between CR and LF.
partitions :: BS.ByteString -> [[BS.ByteString]]
partitions bytes
  | BS.null bytes = [[]]
  | otherwise = [BS.take n bytes : rest | n <- [1 .. BS.length bytes], rest <- partitions (BS.drop n bytes)]

invalid :: [BS.ByteString] -> IO Bool
invalid chunks = do
  result <- try (split chunks) :: IO (Either StreamError [String])
  pure (result == Left (StreamError "lines: invalid or incomplete UTF-8"))

tests :: [(String, IO Bool)]
tests =
  [ ("empty input has no lines", (== []) <$> split [])
  , ("empty chunks have no lines", (== []) <$> split [BS.empty, BS.empty])
  , ("LF framing preserves blank lines", (== ["a", "", "b"]) <$> split [utf8 "a\n\nb\n"])
  , ("an unterminated final line is emitted", (== ["a", "b"]) <$> split [utf8 "a\nb"])
  , ("trailing LF does not add an extra line", (== ["a"]) <$> split [utf8 "a\n"])
  , ("a single LF is one empty line", (== [""]) <$> split [utf8 "\n"])
  , ("CRLF normalises across chunks", (== ["a", "", "b"]) <$> split (map utf8 ["a\r", "\n\r", "\nb\r\n"]))
  , ("an unterminated CR remains data", (== ["a\r"]) <$> split [utf8 "a\r"])
  , ("spaces and NUL remain data", (== [" a \0b "]) <$> split [utf8 " a \0b \n"])
  , ("all chunk partitions produce identical Unicode lines", do
      results <- mapM split (partitions (utf8 "aλ\r\n𐐀\nz"))
      pure (all (== ["aλ", "𐐀", "z"]) results))
  , ("long lines span many chunks", do
      let line = replicate 200000 'x' <> "λ"
          bytes = utf8 (line <> "\n")
          chunks value | BS.null value = []
                       | otherwise = BS.take 4096 value : chunks (BS.drop 4096 value)
      (== [line]) <$> split (chunks bytes))
  , ("invalid leading byte fails", invalid [BS.pack [255, 10]])
  , ("truncated final character fails", invalid [BS.pack [194]])
  , ("invalid continuation across chunks fails", invalid [BS.pack [194], BS.pack [32, 10]])
  , ("overlong UTF-8 fails", invalid [BS.pack [192, 128, 10]])
  , ("UTF-8 surrogate fails", invalid [BS.pack [237, 160, 128, 10]])
  , ("complete lines emit before the next read", do
      emitted <- newIORef []
      calls <- newIORef (0 :: Int)
      let receive = do
            count <- atomicModifyIORef' calls (\n -> (n + 1, n))
            if count == 0 then pure (Just (utf8 "ready\n")) else do
              visible <- readIORef emitted
              unless (visible == ["ready"]) (ioError (userError "line was buffered past the next read"))
              pure Nothing
      runLines receive (\line -> modifyIORef' emitted (line :))
      (== ["ready"]) <$> readIORef emitted)
  ]

main :: IO ()
main = do
  results <- mapM check tests
  putStrLn (show (length (filter id results)) <> "/" <> show (length results) <> " passed")
  unless (and results) exitFailure
 where
  check (name, action) = do
    result <- try (timeout 5000000 action) :: IO (Either SomeException (Maybe Bool))
    let passed = case result of Right (Just True) -> True; _ -> False
    putStrLn ((if passed then "pass  " else "FAIL  ") <> name)
    pure passed
