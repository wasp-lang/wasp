module Wasp.Entity
    ( Entity (..)
    , EntityField (..)
    , EntityFieldType (..)
    ) where

import qualified Data.Text as Text
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), object, ToJSON(..))


data Entity = Entity
    { entityName :: !String
    , entityFields :: ![EntityField]
    } deriving (Show, Eq)

data EntityField = EntityField
    { entityFieldName :: !String
    , entityFieldType :: !EntityFieldType
    } deriving (Show, Eq)

data EntityFieldType = EftString | EftBoolean deriving (Eq)

instance Show EntityFieldType where
    show EftString = "string"
    show EftBoolean = "boolean"

instance ToJSON Entity where
    toJSON entity = object
        [ "name" .= entityName entity
        , "fields" .= entityFields entity
        ]

instance ToJSON EntityField where
    toJSON entityField = object
        [ "name" .= entityFieldName entityField
        , "type" .= entityFieldType entityField
        ]

instance ToJSON EntityFieldType where
    toJSON = Aeson.String . Text.pack . show
