module Wasp.Parser.Action
  ( action,
  )
where

import Data.Maybe (fromMaybe)
import qualified Wasp.Lexer as L
import qualified Wasp.Parser.Common as C
import qualified Wasp.Parser.Operation as Operation
import Text.Parsec.String (Parser)
import Wasp.Wasp.Action (Action)
import qualified Wasp.Wasp.Action as Action

action :: Parser Action
action = do
  (name, props) <- C.waspElementNameAndClosureContent L.reservedNameAction Operation.properties
  return
    Action.Action
      { Action._name = name,
        Action._jsFunction =
          fromMaybe (error "Action js function is missing.") (Operation.getJsFunctionFromProps props),
        Action._entities = Operation.getEntitiesFromProps props,
        Action._auth = Operation.getAuthEnabledFromProps props
      }
