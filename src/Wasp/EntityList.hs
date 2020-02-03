module Wasp.EntityList
    ( EntityList(..)
    , Field(..)
    , getConfigForField
    ) where

import Data.Aeson ((.=), object, ToJSON(..))

import Wasp.JsCode (JsCode)

import qualified Util as U
import qualified Wasp.Entity as Entity


data EntityList = EntityList
    { _name :: !String -- Name of the list
    , _entityName :: !String -- Name of the entity the form is linked to
    , _fields :: [Field]
    } deriving (Show, Eq)

-- NOTE(matija): Ideally generator would not depend on this logic defined outside of it.
-- We are moving away from this approach but some parts of code (Page generator) still
-- rely on it so we cannot remove it completely yet without further refactoring.
--
-- Some record fields are note even included (e.g. _fields), we are keeping this only for the
-- backwards compatibility.
instance ToJSON EntityList where
    toJSON entityList = object
        [ "name" .= _name entityList
        , "entityName" .= _entityName entityList
        ]

-- | For a given entity field, returns its configuration from the given entity-list, if present.
-- TODO(matija): this is very similar to the same function in EntityForm, we could extract it
-- (prob. using typeclass or TH) in the future.
getConfigForField :: EntityList -> Entity.EntityField -> Maybe Field
getConfigForField entityList entityField =
    U.headSafe $ filter isConfigOfInputEntityField $ _fields entityList
    where
        isConfigOfInputEntityField :: Field -> Bool
        isConfigOfInputEntityField =
            (== Entity.entityFieldName entityField) . _fieldName

-- * Field

data Field = Field
    { _fieldName :: !String
    , _fieldRender :: Maybe JsCode -- Js function that renders a list field.
    } deriving (Show, Eq)
