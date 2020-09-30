module Parser.Operation
    ( jsFunctionPropParser
    , entitiesPropParser
    , getJsFunctionFromProps
    , getEntitiesFromProps
    , properties
    -- FOR TESTS:
    , Property(..)
    ) where

import           Data.Maybe         (listToMaybe)
import           Text.Parsec        ((<|>))
import           Text.Parsec.String (Parser)

import qualified Lexer              as L
import qualified Parser.Common      as C
import qualified Parser.JsImport
import qualified Wasp.JsImport


data Property = JsFunction !Wasp.JsImport.JsImport
              | Entities ![String]
    deriving (Show, Eq)

properties :: Parser [Property]
properties = L.commaSep1 $
    jsFunctionPropParser
    <|> entitiesPropParser

jsFunctionPropParser :: Parser Property
jsFunctionPropParser = JsFunction <$> C.waspProperty "fn" Parser.JsImport.jsImport

getJsFunctionFromProps :: [Property] -> Maybe Wasp.JsImport.JsImport
getJsFunctionFromProps ps = listToMaybe [f | JsFunction f <- ps]

entitiesPropParser :: Parser Property
entitiesPropParser = Entities <$> C.waspProperty "entities" (C.waspArray L.identifier)

getEntitiesFromProps :: [Property] -> Maybe [String]
getEntitiesFromProps ps = listToMaybe [es | Entities es <- ps]
