module Parser.Style
  ( style,
  )
where

import qualified Data.Text as Text
import qualified Parser.Common
import qualified Parser.ExternalCode
import Text.Parsec ((<|>))
import Text.Parsec.String (Parser)
import qualified Wasp.Style

style :: Parser Wasp.Style.Style
style = cssFile <|> cssCode

cssFile :: Parser Wasp.Style.Style
cssFile = Wasp.Style.ExtCodeCssFile <$> Parser.ExternalCode.extCodeFilePathString

cssCode :: Parser Wasp.Style.Style
cssCode = (Wasp.Style.CssCode . Text.pack) <$> Parser.Common.waspNamedClosure "css"
