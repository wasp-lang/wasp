module Wasp.Util.Aeson
  ( encodeToText,
    decodeFromString,
    encodeToString,
  )
where

import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.Aeson.Text (encodeToTextBuilder)
import qualified Data.ByteString.Lazy.UTF8 as BS
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)

encodeToText :: (ToJSON a) => a -> Text
encodeToText = toStrict . toLazyText . encodeToTextBuilder

decodeFromString :: (FromJSON a) => String -> Either String a
decodeFromString = eitherDecode . BS.fromString

encodeToString :: (ToJSON a) => a -> String
encodeToString = BS.toString . encode
