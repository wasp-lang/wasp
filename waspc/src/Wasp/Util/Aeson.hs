module Wasp.Util.Aeson
  ( decodeFromString,
    encodeToString,
  )
where

import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString.Lazy.UTF8 as BS

decodeFromString :: (FromJSON a) => String -> Either String a
decodeFromString = eitherDecode . BS.fromString

encodeToString :: (ToJSON a) => a -> String
encodeToString = BS.toString . encode
