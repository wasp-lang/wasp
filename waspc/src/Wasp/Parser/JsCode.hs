module Wasp.Parser.JsCode
  ( jsCode,
  )
where

import qualified Data.Text as Text
import Text.Parsec.String (Parser)
import qualified Wasp.Parser.Common as P
import qualified Wasp.Wasp.JsCode as WJS

jsCode :: Parser WJS.JsCode
jsCode = (WJS.JsCode . Text.pack) <$> P.waspNamedClosure "js"
