-- | LF framing with strict UTF-8 decoding. Keep byte fragments until a line
-- ends, so a multibyte character can cross any number of input chunks. Each
-- fragment is copied only when a complete line is assembled.
module Lambdash.Lines (runLines) where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Lambdash.Stream (StreamError (..))

runLines :: IO (Maybe BS.ByteString) -> (String -> IO ()) -> IO ()
runLines receive emit = loop []
 where
  loop fragments = receive >>= maybe (finish fragments) (feed fragments)
  feed fragments chunk =
    let (prefix, rest) = BS.break (== 10) chunk
        pieces = if BS.null prefix then fragments else prefix : fragments
    in if BS.null rest
      then loop pieces
      else do
        let line = BS.concat (reverse pieces)
            normalized = if not (BS.null line) && BS.last line == 13 then BS.init line else line
        decode normalized >>= emit
        feed [] (BS.tail rest)
  finish fragments = unless (null fragments) (decode (BS.concat (reverse fragments)) >>= emit)
  decode bytes = case Text.decodeUtf8' bytes of
    Left _ -> throwIO (StreamError "lines: invalid or incomplete UTF-8")
    Right text -> pure (Text.unpack text)
