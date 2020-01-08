module Parser.JsImport
    ( jsImport
    ) where

import Text.Parsec (manyTill, anyChar, try)
import Text.Parsec.Char (space)
import Text.Parsec.String (Parser)

import qualified Lexer as L
import Wasp.JsImport as JsImport

-- import ... from "..."
jsImport :: Parser JsImport.JsImport
jsImport = do
    L.whiteSpace
    _ <- L.reserved L.reservedNameImport
    -- TODO: In the future, we could further tighten up this parser so it strictly follows format of js import statement.
    --   Right now it is more relaxed -> it allows almost anything between "import" and "from".
    what <- anyChar `manyTill` (try (space *> L.whiteSpace *> L.reserved L.reservedNameFrom))
    -- TODO: For now we only support double quotes here, we should also support single quotes.
    --   We would need to write this from scratch, with single quote escaping enabled.
    from <- L.stringLiteral
    return JsImport.JsImport { JsImport.jsImportWhat = what, JsImport.jsImportFrom = from }
