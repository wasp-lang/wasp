module Parser.Action
    ( action
    ) where

import Text.Parsec.String (Parser)
import Data.Maybe (listToMaybe, fromMaybe)

import qualified Wasp.Action
import qualified Wasp.JsImport

import qualified Parser.JsImport
import qualified Parser.Common as C
import qualified Lexer as L

-- TODO: Right now, this is mostly just duplicated Parser.Query.
--   In the future, consider if we should generalize these two somehow
--   to reduce the duplication.

-- | Parses action looking like this:
-- action myAction {
--   fn: import { myActionInJs } from "..."
-- }
action :: Parser Wasp.Action.Action
action = do
    (actionName, actionProps) <- C.waspElementNameAndClosureContent L.reservedNameAction actionProperties
    return Wasp.Action.Action
        { Wasp.Action._name = actionName
        , Wasp.Action._jsFunction =
            fromMaybe (error "Action js function is missing.") (getActionJsFunction actionProps)
        }

data ActionProperty = JsFunction !Wasp.JsImport.JsImport
    deriving (Show, Eq)

actionProperties :: Parser [ActionProperty]
actionProperties = L.commaSep1 actionPropertyJsFunction

actionPropertyJsFunction :: Parser ActionProperty
actionPropertyJsFunction = JsFunction <$> C.waspProperty "fn" Parser.JsImport.jsImport

getActionJsFunction :: [ActionProperty] -> Maybe Wasp.JsImport.JsImport
getActionJsFunction ps = listToMaybe [f | JsFunction f <- ps]
