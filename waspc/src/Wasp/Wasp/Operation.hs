module Wasp.Operation
  ( Operation (..),
    getName,
    getJsFn,
    getEntities,
    getAuth,
  )
where

-- TODO: Is this ok approach, should I instead use typeclass?
--   So far, all usages in the codebase could be easily replaced with the Typeclass.

import Wasp.Action (Action)
import qualified Wasp.Action as Action
import Wasp.JsImport (JsImport)
import Wasp.Query (Query)
import qualified Wasp.Query as Query

data Operation
  = QueryOp Query
  | ActionOp Action
  deriving (Show)

getName :: Operation -> String
getName (QueryOp query) = Query._name query
getName (ActionOp action) = Action._name action

getJsFn :: Operation -> JsImport
getJsFn (QueryOp query) = Query._jsFunction query
getJsFn (ActionOp action) = Action._jsFunction action

getEntities :: Operation -> Maybe [String]
getEntities (QueryOp query) = Query._entities query
getEntities (ActionOp action) = Action._entities action

getAuth :: Operation -> Maybe Bool
getAuth (QueryOp query) = Query._auth query
getAuth (ActionOp action) = Action._auth action
