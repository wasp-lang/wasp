module Parser.JsImport
    ( jsImport
    ) where

import Text.Parsec ((<|>))
import Text.Parsec.String (Parser)

import qualified Parser.ExternalCode
import qualified Lexer as L
import qualified Wasp.JsImport


-- | Parses subset of JS import statement (only default or single named import, and only external code files):
--   import <identifier> from "@ext/..."
--   import { <identifier> } from "@ext/..."
jsImport :: Parser Wasp.JsImport.JsImport
jsImport = do
    L.whiteSpace
    _ <- L.reserved L.reservedNameImport
    -- For now we support only default import or one named import.
    (defaultImport, namedImports) <- ((\i -> (Just i, [])) <$> L.identifier)
                                     <|> ((\i -> (Nothing, [i])) <$> L.braces L.identifier)
    _ <- L.reserved L.reservedNameFrom
    -- TODO: For now we only support double quotes here, we should also support single quotes.
    --   We would need to write this from scratch, with single quote escaping enabled.
    from <- Parser.ExternalCode.extCodeFilePathString
    return Wasp.JsImport.JsImport
        { Wasp.JsImport.jsImportDefaultImport = defaultImport
        , Wasp.JsImport.jsImportNamedImports = namedImports
        , Wasp.JsImport.jsImportFrom = from
        }
