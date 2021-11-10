module Parser.JsCode
  ( jsCode,
  )
where

import qualified Data.Text as Text
import qualified Parser.Common as P
import Text.Parsec.String (Parser)
import qualified Wasp.JsCode as WJS

jsCode :: Parser WJS.JsCode
jsCode = (WJS.JsCode . Text.pack) <$> P.waspNamedClosure "js"
