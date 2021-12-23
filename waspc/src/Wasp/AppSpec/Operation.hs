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
  = QueryOp String Query
  | ActionOp String Action
  deriving (Show)

getName :: Operation -> String
getName (QueryOp name _) = name
getName (ActionOp name _) = name

getFn :: Operation -> ExtImport
getFn (QueryOp _ query) = Query.fn query
getFn (ActionOp _ action) = Action.fn action

getEntities :: Operation -> Maybe [Ref Entity]
getEntities (QueryOp _ query) = Query.entities query
getEntities (ActionOp _ action) = Action.entities action

getAuth :: Operation -> Maybe Bool
getAuth (QueryOp _ query) = Query.auth query
getAuth (ActionOp _ action) = Action.auth action
