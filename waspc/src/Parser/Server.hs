module Parser.Server
  ( server,
  )
where

import Data.Maybe (fromMaybe, listToMaybe)
import qualified Lexer as L
import qualified Parser.Common as C
import qualified Parser.JsImport
import Text.Parsec.String (Parser)
import qualified Wasp.JsImport
import Wasp.Server (Server)
import qualified Wasp.Server as Server

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
setupJsFunctionPropParser = SetupJsFunction <$> C.waspProperty "setupFn" Parser.JsImport.jsImport

getSetupJsFunctionFromProps :: [Property] -> Maybe Wasp.JsImport.JsImport
getSetupJsFunctionFromProps ps = listToMaybe [f | SetupJsFunction f <- ps]
