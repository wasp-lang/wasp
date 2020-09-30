module Wasp.Action
    ( Action(..)
    ) where

import           Data.Aeson    (ToJSON (..), object, (.=))
import           Wasp.JsImport (JsImport)

-- TODO: Very similar to Wasp.Query, consider extracting duplication.

data Action = Action
    { _name       :: !String
    , _jsFunction :: !JsImport
    , _entities   :: !(Maybe [String])
    } deriving (Show, Eq)

instance ToJSON Action where
    toJSON action = object
        [ "name" .= _name action
        , "jsFunction" .= _jsFunction action
        , "entities" .= _entities action
        ]
