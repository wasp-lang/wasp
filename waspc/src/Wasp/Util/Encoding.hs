module Wasp.Util.Encoding
  ( decodeLocaleEncoding,
    decodeEncoding,
  )
where

import Control.Monad.State (evalStateT)
import Data.ByteString (ByteString)
import qualified Data.Encoding as E
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeLatin1, decodeUtf8With)
import GHC.IO.Encoding (TextEncoding, textEncodingName)
import System.Directory.Internal.Prelude (fromMaybe)
import System.IO (latin1, localeEncoding, utf8)

-- | Decodes given byte string while assuming it is using locale encoding (which is machine's default encoding).
decodeLocaleEncoding :: ByteString -> Text
decodeLocaleEncoding = decodeEncoding localeEncoding

-- | Decodes given byte string while assuming it is using provided text encoding.
decodeEncoding :: TextEncoding -> ByteString -> Text
decodeEncoding enc = fromMaybe unsupportedEncoderError decoder
  where
    decoder = getTextLibDecoder enc <> getEncodingLibDecoder enc
    unsupportedEncoderError = error $ "Wasp doesn't know how to decode " ++ textEncodingName enc ++ " encoding."

-- | Check if there is a decoder for given encoding in the @text@ package, and if so, returns it.
getTextLibDecoder :: TextEncoding -> Maybe (ByteString -> Text)
getTextLibDecoder enc
  | textEncodingName enc == textEncodingName latin1 = Just decodeLatin1
  | textEncodingName enc == textEncodingName utf8 = Just decodeUtf8Lenient
  | otherwise = Nothing
  where
    decodeUtf8Lenient :: ByteString -> Text
    decodeUtf8Lenient =
      let onErrorUseReplacementChar _ _ = Just '\xfffd' -- This will replace any invalid characters with \xfffd.
       in decodeUtf8With onErrorUseReplacementChar

-- | Check if there is a decoder for given encoding in the @encoding@ package, and if so, returns it.
-- @encoding@ package brings support for many more encodings to Haskell.
getEncodingLibDecoder :: TextEncoding -> Maybe (ByteString -> Text)
getEncodingLibDecoder enc = makeDecoder <$> E.encodingFromStringExplicit (textEncodingName enc)
  where
    makeDecoder :: E.DynEncoding -> ByteString -> Text
    makeDecoder dynEnc =
      \bs -> case E.decode dynEnc `evalStateT` bs of
        Right decoded -> T.pack decoded
        Left (e :: E.DecodingException) ->
          error $ "Decoding " ++ textEncodingName localeEncoding ++ " bytestring failed: " ++ show e
