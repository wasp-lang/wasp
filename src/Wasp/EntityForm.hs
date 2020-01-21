module Wasp.EntityForm
    ( EntityForm(..)
    , Submit(..)
    , SubmitButton(..)
    ) where

import Data.Aeson ((.=), object, ToJSON(..))

data EntityForm = EntityForm
    { _name :: !String -- Name of the form
    , _entityName :: !String -- Name of the entity the form is linked to
    -- TODO(matija): should we make these maybes also strict?
    , _submit :: Maybe Submit
    } deriving (Show, Eq)

data Submit = Submit
    { _onEnter :: Maybe Bool
    , _submitButton :: Maybe SubmitButton
    } deriving (Show, Eq)

data SubmitButton = SubmitButton
    { _show :: Maybe Bool
    } deriving (Show, Eq)

instance ToJSON EntityForm where
    toJSON entityForm = object
        [ "name" .= _name entityForm
        , "entityName" .= _entityName entityForm
        , "submitConfig" .= _submit entityForm
        ]

instance ToJSON Submit where
    toJSON submit = object
        [ "onEnter" .= _onEnter submit
        ]
