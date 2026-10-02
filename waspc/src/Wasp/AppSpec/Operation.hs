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
getName (QueryOp query) = query.name
getName (ActionOp action) = action.name

getFn :: Operation -> ExtImport
getFn (QueryOp query) = query.fn
getFn (ActionOp action) = action.fn

getEntities :: Operation -> Maybe [Ref Entity]
getEntities (QueryOp query) = query.entities
getEntities (ActionOp action) = action.entities

getAuth :: Operation -> Maybe Bool
getAuth (QueryOp query) = query.auth
getAuth (ActionOp action) = action.auth
