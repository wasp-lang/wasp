{-# LANGUAGE TypeApplications #-}

module Wasp.Util.Encoding
  ( decodeWithTE,
    decodeWithTELenient,
    encodeWithTE,
  )
where

import Control.DeepSeq (force)
import Control.Exception (Exception (displayException), SomeException, evaluate, try)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (isSuffixOf)
import qualified GHC.Foreign as GHC
import GHC.IO (unsafePerformIO)
import GHC.IO.Encoding (mkTextEncoding)
import GHC.IO.Encoding.Types (TextEncoding (textEncodingName))

-- decodeWithTE and encodeWithTE are modeled by this reddit comment:
--   https://www.reddit.com/r/haskell/comments/vrw476/comment/iezwd0t/?utm_source=share&utm_medium=web2x&context=3
-- which points to this code in Filepath (with some small modifications):
--   https://gitlab.haskell.org/haskell/filepath/-/blob/master/System/OsPath/Encoding/Internal.hs#L298

type EncodingError = String

-- | Decode with the given 'TextEncoding'.
decodeWithTE :: TextEncoding -> ByteString -> Either EncodingError String
decodeWithTE enc bs = unsafePerformIO $ do
  decodedOrError <- decodeWithTEIO enc bs
  evaluate $ force decodedOrError

-- | Same like decodeWithTE, but it will choose a replacement character for illegal sequences or code points.
decodeWithTELenient :: TextEncoding -> ByteString -> String
decodeWithTELenient enc bs = unsafePerformIO $ do
  encTranslit <- mkTextEncoding $ textEncodingName enc `addSuffixIfMissing` "//TRANSLIT"
  decodedOrError <- decodeWithTEIO encTranslit bs
  evaluate $ force $ fromRight' error decodedOrError

decodeWithTEIO :: TextEncoding -> ByteString -> IO (Either EncodingError String)
decodeWithTEIO enc bs = do
  decodedStrOrError <- try @SomeException $ BS.useAsCStringLen bs $ GHC.peekCStringLen enc
  return $ first displayException decodedStrOrError

-- | Encode with the given 'TextEncoding'.
encodeWithTE :: TextEncoding -> String -> Either EncodingError ByteString
encodeWithTE enc str = unsafePerformIO $ do
  encodedBsOrError <- try @SomeException $ GHC.withCStringLen enc str BS.packCStringLen
  evaluate $ force $ first displayException encodedBsOrError

fromRight' :: (a -> b) -> Either a b -> b
fromRight' f (Left x) = f x
fromRight' _ (Right x) = x

addSuffixIfMissing :: (Eq a) => [a] -> [a] -> [a]
addSuffixIfMissing t suffix = if suffix `isSuffixOf` t then t else t <> suffix
