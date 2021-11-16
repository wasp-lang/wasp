module Wasp.Parser.Style
  ( style,
  )
where

import qualified Data.Text as Text
import Text.Parsec ((<|>))
import Text.Parsec.String (Parser)
import qualified Wasp.Parser.Common
import qualified Wasp.Parser.ExternalCode
import qualified Wasp.Wasp.Style as Wasp.Style

style :: Parser Wasp.Style.Style
style = cssFile <|> cssCode

cssFile :: Parser Wasp.Style.Style
cssFile = Wasp.Style.ExtCodeCssFile <$> Wasp.Parser.ExternalCode.extCodeFilePathString

cssCode :: Parser Wasp.Style.Style
cssCode = Wasp.Style.CssCode . Text.pack <$> Wasp.Parser.Common.waspNamedClosure "css"
