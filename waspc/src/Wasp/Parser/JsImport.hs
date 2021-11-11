module Wasp.Parser.JsImport
  ( jsImport,
  )
where

import Text.Parsec ((<|>))
import Text.Parsec.String (Parser)
import qualified Wasp.Lexer as L
import qualified Wasp.Parser.ExternalCode
import qualified Wasp.Wasp.JsImport as Wasp.JsImport

-- | Parses subset of JS import statement (only default or single named import, and only external code files):
--   import <identifier> from "@ext/..."
--   import { <identifier> } from "@ext/..."
jsImport :: Parser Wasp.JsImport.JsImport
jsImport = do
  L.whiteSpace
  _ <- L.reserved L.reservedNameImport
  -- For now we support only default import or one named import.
  (defaultImport, namedImports) <-
    ((\i -> (Just i, [])) <$> L.identifier)
      <|> ((\i -> (Nothing, [i])) <$> L.braces L.identifier)
  _ <- L.reserved L.reservedNameFrom
  -- TODO: For now we only support double quotes here, we should also support single quotes.
  --   We would need to write this from scratch, with single quote escaping enabled.
  from <- Wasp.Parser.ExternalCode.extCodeFilePathString
  return
    Wasp.JsImport.JsImport
      { Wasp.JsImport._defaultImport = defaultImport,
        Wasp.JsImport._namedImports = namedImports,
        Wasp.JsImport._from = from
      }
