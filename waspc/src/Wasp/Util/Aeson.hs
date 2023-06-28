module Wasp.Util.Aeson
  ( encodeToText,
  )
where

import Data.Aeson (ToJSON)
import Data.Aeson.Text (encodeToTextBuilder)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)

encodeToText :: ToJSON a => a -> Text
encodeToText = toStrict . toLazyText . encodeToTextBuilder
