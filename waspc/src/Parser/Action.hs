module Parser.Action
    ( action
    ) where

import           Data.Maybe         (fromMaybe)
import           Text.Parsec.String (Parser)

import qualified Lexer              as L
import qualified Parser.Common      as C
import qualified Parser.Operation   as Operation
import           Wasp.Action         (Action)
import qualified Wasp.Action         as Action


action :: Parser Action
action = do
    (name, props) <- C.waspElementNameAndClosureContent L.reservedNameAction Operation.properties
    return Action.Action
        { Action._name = name
        , Action._jsFunction =
            fromMaybe (error "Action js function is missing.") (Operation.getJsFunctionFromProps props)
        , Action._entities = Operation.getEntitiesFromProps props
        }
