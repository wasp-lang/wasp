module Wasp.AppSpec.Operation
  ( Operation (..),
    getName,
    getFn,
    getEntities,
    getAuth,
  )
where

import Wasp.AppSpec.Action (Action)
import qualified Wasp.AppSpec.Action as Action
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.Entity (Entity)
import Wasp.AppSpec.ExtImport (ExtImport)
import Wasp.AppSpec.Query (Query)
import qualified Wasp.AppSpec.Query as Query

-- | Common "interface" for queries and actions.
data Operation
  = QueryOp Query
  | ActionOp Action
  deriving (Show)

getName :: Operation -> String
getName (QueryOp query) = Query.name query
getName (ActionOp action) = Action.name action

getFn :: Operation -> ExtImport
getFn (QueryOp query) = Query.fn query
getFn (ActionOp action) = Action.fn action

getEntities :: Operation -> Maybe [Ref Entity]
getEntities (QueryOp query) = Query.entities query
getEntities (ActionOp action) = Action.entities action

getAuth :: Operation -> Maybe Bool
getAuth (QueryOp query) = Query.auth query
getAuth (ActionOp action) = Action.auth action
