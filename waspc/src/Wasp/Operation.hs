module Wasp.Operation
    ( Operation(..)
    , getName
    , getJsFn
    ) where

-- TODO: Is this ok approach, should I instead use typeclass?
--   So far, all usages in the codebase could be easily replaced with the Typeclass.

import Wasp.JsImport (JsImport)
import Wasp.Query (Query)
import qualified Wasp.Query as Query
import Wasp.Action (Action)
import qualified Wasp.Action as Action

data Operation = QueryOp Query
               | ActionOp Action

getName :: Operation -> String
getName (QueryOp query) = Query._name query
getName (ActionOp action) = Action._name action

getJsFn :: Operation -> JsImport
getJsFn (QueryOp query) = Query._jsFunction query
getJsFn (ActionOp action) = Action._jsFunction action
