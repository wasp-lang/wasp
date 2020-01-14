module Wasp.EntityForm
    ( EntityForm(..)
    , EntityFormSubmitConfig(..)
    ) where

import Data.Aeson ((.=), object, ToJSON(..))

data EntityForm = EntityForm
    { efName :: !String -- Name of the form
    , efEntityName :: !String -- Name of the entity the form is linked to
    , efSubmitConfig :: Maybe EntityFormSubmitConfig
    } deriving (Show, Eq)

data EntityFormSubmitConfig = EntityFormSubmitConfig
    { onEnter :: !Bool
    } deriving (Show, Eq)

instance ToJSON EntityForm where
    toJSON entityForm = object
        [ "name" .= efName entityForm
        , "entityName" .= efEntityName entityForm
        , "submitConfig" .= efSubmitConfig entityForm
        ]

instance ToJSON EntityFormSubmitConfig where
    toJSON submitConfig = object
        [ "onEnter" .= onEnter submitConfig
        ]
