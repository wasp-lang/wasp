module Wasp.Action
    ( Action(..)
    ) where

import Data.Aeson ((.=), object, ToJSON(..))
import Wasp.JsCode (JsCode)

-- Although name is general (Action), we are actually making many implicit assumptions here:
-- That Action is an update action that updates the whole collection of certain entity at once.
-- We are doing this because it is general enough for our purposes right now, but in the future we
-- will want to have much more developed system of actions, and Action will probably only be the
-- umbrella name/type for all of the action types.

data Action = Action
    { _name :: !String
    , _entityName :: !String
    -- | Js is expected to be function that takes list of entities and returns new list of entities.
    , _updateFn :: !JsCode
    } deriving (Show, Eq)

instance ToJSON Action where
    toJSON action = object
        [ "name" .= _name action
        , "entityName" .= _entityName action
        , "updateFn" .= _updateFn action
        ]
