module Parser.Query
    ( query
    ) where

import Text.Parsec.String (Parser)
import Data.Maybe (listToMaybe, fromMaybe)

import qualified Wasp.Query
import qualified Wasp.JsImport

import qualified Parser.JsImport
import qualified Parser.Common as C
import qualified Lexer as L


-- | Parses query looking like this:
-- query myQuery {
--   fn: import { myQueryInJs } from "..."
-- }
query :: Parser Wasp.Query.Query
query = do
    (queryName, queryProps) <- C.waspElementNameAndClosure L.reservedNameQuery queryProperties
    return Wasp.Query.Query
        { Wasp.Query._name = queryName
        , Wasp.Query._jsFunction =
            fromMaybe (error "Query js function is missing.") (getQueryJsFunction queryProps)
        }

data QueryProperty = JsFunction !Wasp.JsImport.JsImport
    deriving (Show, Eq)

queryProperties :: Parser [QueryProperty]
queryProperties = L.commaSep1 queryPropertyJsFunction

queryPropertyJsFunction :: Parser QueryProperty
queryPropertyJsFunction = JsFunction <$> C.waspProperty "fn" Parser.JsImport.jsImport

getQueryJsFunction :: [QueryProperty] -> Maybe Wasp.JsImport.JsImport
getQueryJsFunction ps = listToMaybe [f | JsFunction f <- ps]
