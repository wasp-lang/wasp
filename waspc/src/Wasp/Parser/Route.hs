module Wasp.Parser.Route
  ( route,
  )
where

import qualified Wasp.Lexer as L
import Text.Parsec.String (Parser)
import qualified Wasp.Wasp.Route as Route

-- | Top level parser, parses route Wasp element.
route :: Parser Route.Route
route = do
  -- route "some/url/path"
  L.reserved L.reservedNameRoute
  urlPath <- L.stringLiteral

  -- -> page somePage
  L.reserved "->"
  L.reserved L.reservedNamePage
  targetPage <- L.identifier

  return
    Route.Route
      { Route._urlPath = urlPath,
        Route._targetPage = targetPage
      }
