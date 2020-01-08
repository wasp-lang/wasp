module Wasp.EntityForm
    ( EntityForm(..)
    , EntityFormSubmitConfig(..)
    ) where


data EntityForm = EntityForm
    { efName :: !String -- Name of the form
    , efEntityName :: !String -- Name of the entity the form is linked to
    , efSubmitConfig :: Maybe EntityFormSubmitConfig
    } deriving (Show, Eq)

data EntityFormSubmitConfig = EntityFormSubmitConfig
    { onEnter :: !Bool
    } deriving (Show, Eq)
