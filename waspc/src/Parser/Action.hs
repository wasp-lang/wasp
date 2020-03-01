module Parser.Action
    ( action
    ) where

import Text.Parsec.String (Parser)

import qualified Wasp.Action as Action

import qualified Parser.Common as Common
import qualified Parser.JsCode as JsCode
import qualified Lexer as L


action :: Parser Action.Action
action = do
    (entityName, actionName, updateFn) <- Common.waspElementLinkedToEntity L.reservedNameAction JsCode.jsCode
    return Action.Action
        { Action._name = actionName
        , Action._entityName = entityName
        , Action._updateFn = updateFn
        }
