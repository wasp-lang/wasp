module Wasp.EntityList
    ( EntityList(..)
    ) where

import Data.Aeson ((.=), object, ToJSON(..))

data EntityList = EntityList
    { _name :: !String -- Name of the list
    , _entityName :: !String -- Name of the entity the form is linked to
    } deriving (Show, Eq)

instance ToJSON EntityList where
    toJSON entityList = object
        [ "name" .= _name entityList
        , "entityName" .= _entityName entityList
        ]
