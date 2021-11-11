module Wasp.Parser.Server
  ( server,
  )
where

import Data.Maybe (fromMaybe, listToMaybe)
import Text.Parsec.String (Parser)
import qualified Wasp.Lexer as L
import qualified Wasp.Parser.Common as C
import qualified Wasp.Parser.JsImport
import qualified Wasp.Wasp.JsImport as Wasp.JsImport
import Wasp.Wasp.Server (Server)
import qualified Wasp.Wasp.Server as Server

server :: Parser Server
server = do
  L.reserved L.reservedNameServer
  props <- C.waspClosure properties

  return
    Server.Server
      { Server._setupJsFunction =
          fromMaybe (error "Server js function is missing.") (getSetupJsFunctionFromProps props)
      }

data Property
  = SetupJsFunction !Wasp.JsImport.JsImport
  deriving (Show, Eq)

properties :: Parser [Property]
properties =
  L.commaSep1 setupJsFunctionPropParser

setupJsFunctionPropParser :: Parser Property
setupJsFunctionPropParser = SetupJsFunction <$> C.waspProperty "setupFn" Wasp.Parser.JsImport.jsImport

getSetupJsFunctionFromProps :: [Property] -> Maybe Wasp.JsImport.JsImport
getSetupJsFunctionFromProps ps = listToMaybe [f | SetupJsFunction f <- ps]
