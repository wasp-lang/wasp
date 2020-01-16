module Generator.Entity.Common
    ( entityTemplatesDirPath
    , entityDirPathInSrc
    , entityTemplateData
    , entityComponentsDirPathInSrc
    ) where

import System.FilePath (FilePath, (</>))
import Data.Aeson ((.=), object)
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text

import qualified Util
import Wasp

-- | Path of the entity-related generated code, relative to src/ directory.
entityDirPathInSrc :: Entity -> FilePath
entityDirPathInSrc entity = "entities" </> Util.camelToKebabCase (entityName entity)

-- Path of the code generated for entity components, relative to src/ directory.
entityComponentsDirPathInSrc :: Entity -> FilePath
entityComponentsDirPathInSrc entity = (entityDirPathInSrc entity) </> "components"

-- | Location in templates where entity related templates reside.
entityTemplatesDirPath :: FilePath
entityTemplatesDirPath = "src" </> "entities" </> "_entity"

-- | Default generic data for entity templates.
entityTemplateData :: Wasp -> Entity -> Aeson.Value
entityTemplateData wasp entity = object
    [ "wasp" .= wasp
    , "entity" .= entity
    , "entityLowerName" .= (Util.toLowerFirst $ entityName entity)
    -- TODO: use it also when creating Class file itself and in other files.
    , "entityClassName" .= (Util.toUpperFirst $ entityName entity)
    , "entityTypedFields" .= map entityFieldToJsonWithTypeAsKey (entityFields entity)
    ]

{- | Converts entity field to a JSON where field type is a key to the object holding
all the other properties. E.g. a field of type boolean could look this as JSON:

{ boolean: { name: "description", type: "boolean" }, name: "description" }

This method is needed to achieve conditional rendering with Mustache. We also add "name"
property again along with the type because it is otherwise not accessible outside of
a specific conditional section.
-}
entityFieldToJsonWithTypeAsKey :: EntityField -> Aeson.Value
entityFieldToJsonWithTypeAsKey entityField = object
    -- TODO(matija): it would be cleaner to have a flat structure, like
    -- { boolean: true, type: "boolean", name: "description" }
    [ (toText $ entityFieldType entityField) .= entityField
    , "name" .= entityFieldName entityField
    ]
  where
    toText = Text.pack . show
