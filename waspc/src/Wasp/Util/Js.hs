module Wasp.Util.Js
  ( makeJsStringLiteral,
  )
where

import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy as TL

-- | Renders a string as a double-quoted JS string literal.
-- Strings that reach the generated code can come from the user (paths, names,
-- cron expressions, ...), so every character that could end the literal early
-- or break the line has to be escaped. A JSON string is a valid JS string
-- literal, so we let the JSON encoder do it.
--
-- >>> makeJsStringLiteral "/say\"hi"
-- "\"/say\\\"hi\""
makeJsStringLiteral :: String -> String
makeJsStringLiteral = TL.unpack . encodeToLazyText
