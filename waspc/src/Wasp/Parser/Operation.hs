module Wasp.Parser.Operation
  ( jsFunctionPropParser,
    entitiesPropParser,
    getJsFunctionFromProps,
    getEntitiesFromProps,
    getAuthEnabledFromProps,
    properties,
    -- FOR TESTS:
    Property (..),
  )
where

import Data.Maybe (listToMaybe)
import Text.Parsec ((<|>))
import Text.Parsec.String (Parser)
import qualified Wasp.Lexer as L
import qualified Wasp.Parser.Common as C
import qualified Wasp.Parser.JsImport
import qualified Wasp.Wasp.JsImport as Wasp.JsImport

data Property
  = JsFunction !Wasp.JsImport.JsImport
  | Entities ![String]
  | AuthEnabled !Bool
  deriving (Show, Eq)

properties :: Parser [Property]
properties =
  L.commaSep1 $
    jsFunctionPropParser
      <|> entitiesPropParser
      <|> authEnabledPropParser

jsFunctionPropParser :: Parser Property
jsFunctionPropParser = JsFunction <$> C.waspProperty "fn" Wasp.Parser.JsImport.jsImport

getJsFunctionFromProps :: [Property] -> Maybe Wasp.JsImport.JsImport
getJsFunctionFromProps ps = listToMaybe [f | JsFunction f <- ps]

entitiesPropParser :: Parser Property
entitiesPropParser = Entities <$> C.waspProperty "entities" (C.waspList L.identifier)

getEntitiesFromProps :: [Property] -> Maybe [String]
getEntitiesFromProps ps = listToMaybe [es | Entities es <- ps]

authEnabledPropParser :: Parser Property
authEnabledPropParser = AuthEnabled <$> C.waspProperty "auth" L.bool

getAuthEnabledFromProps :: [Property] -> Maybe Bool
getAuthEnabledFromProps ps = listToMaybe [aE | AuthEnabled aE <- ps]
