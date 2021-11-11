module Wasp.Parser.Query
  ( query,
  )
where

import Data.Maybe (fromMaybe)
import Text.Parsec.String (Parser)
import qualified Wasp.Lexer as L
import qualified Wasp.Parser.Common as C
import qualified Wasp.Parser.Operation as Operation
import Wasp.Wasp.Query (Query)
import qualified Wasp.Wasp.Query as Query

query :: Parser Query
query = do
  (name, props) <- C.waspElementNameAndClosureContent L.reservedNameQuery Operation.properties
  return
    Query.Query
      { Query._name = name,
        Query._jsFunction =
          fromMaybe (error "Query js function is missing.") (Operation.getJsFunctionFromProps props),
        Query._entities = Operation.getEntitiesFromProps props,
        Query._auth = Operation.getAuthEnabledFromProps props
      }
