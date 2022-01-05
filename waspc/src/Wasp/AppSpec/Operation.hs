module Wasp.AppSpec.Operation
  ( Operation (..),
    getName,
    getFn,
    getEntities,
    getAuth,
  )
where

-- TODO: Is this ok approach, should I instead use typeclass?
--   So far, all usages in the codebase could be easily replaced with the Typeclass.

-- TODO: I copied this file directly from Wasp.Wasp, with slight modifications.
-- It is not really a part of AppSpec, it is not needed to build it, instead it is redundant.
-- I wonder if we should leave it in AppSpec, or move it somewhere else, e.g. to Generator, where
-- it is actually used when we need a common interface for both queries and actions.

import Wasp.AppSpec.Action (Action)
import qualified Wasp.AppSpec.Action as Action
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.Entity (Entity)
import Wasp.AppSpec.ExtImport (ExtImport)
import Wasp.AppSpec.Query (Query)
import qualified Wasp.AppSpec.Query as Query

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
