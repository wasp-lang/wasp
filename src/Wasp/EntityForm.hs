module Wasp.EntityForm
    ( EntityForm(..)
    , Submit(..)
    , SubmitButton(..)
    , Field(..)
    , DefaultValue(..)
    ) where

import Data.Aeson ((.=), object, ToJSON(..))

data EntityForm = EntityForm
    { _name :: !String -- Name of the form
    , _entityName :: !String -- Name of the entity the form is linked to
    -- TODO(matija): should we make these maybes also strict?
    , _submit :: Maybe Submit
    , _fields :: [Field]
    } deriving (Show, Eq)

instance ToJSON EntityForm where
    toJSON entityForm = object
        [ "name" .= _name entityForm
        , "entityName" .= _entityName entityForm
        , "submitConfig" .= _submit entityForm
        ]

-- * Submit

data Submit = Submit
    { _onEnter :: Maybe Bool
    , _submitButton :: Maybe SubmitButton
    } deriving (Show, Eq)

data SubmitButton = SubmitButton
    { _submitButtonShow :: Maybe Bool
    } deriving (Show, Eq)

instance ToJSON Submit where
    toJSON submit = object
        [ "onEnter" .= _onEnter submit
        ]

-- * Field

data Field = Field
    { _fieldName :: !String
    , _fieldShow :: Maybe Bool
    , _fieldDefaultValue :: Maybe DefaultValue
    } deriving (Show, Eq)

data DefaultValue
    = DefaultValueString String
    | DefaultValueBool Bool
    deriving (Show, Eq)


