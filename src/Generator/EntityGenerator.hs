module Generator.EntityGenerator
       ( generateEntities

       , entityDirPathInSrc
       , entityClassPathInSrc
       , entityStatePathInSrc
       , entityActionsPathInSrc
       , entityCreateFormPathInSrc
       , entityListPathInSrc

       -- EXPORTED FOR TESTING:
       , generateEntityClass
       , generateEntityState
       , generateEntityActions
       , generateEntityActionTypes
       , entityTemplatesDirPath
       ) where

import Data.Aeson ((.=), object)
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import System.FilePath (FilePath, (</>), (<.>))

import qualified Util
import Wasp
import Generator.FileDraft


generateEntities :: Wasp -> [FileDraft]
generateEntities wasp = concat $ generateEntity wasp <$> getEntities wasp

generateEntity :: Wasp -> Entity -> [FileDraft]
generateEntity wasp entity =
    [ generateEntityClass wasp entity
    , generateEntityState wasp entity
    , generateEntityActionTypes wasp entity
    , generateEntityActions wasp entity
    ]
    ++ generateEntityComponents wasp entity

generateEntityClass :: Wasp -> Entity -> FileDraft
generateEntityClass wasp entity
    = createSimpleEntityFileDraft wasp entity (entityClassPathInSrc entity) "_Entity.js"

generateEntityState :: Wasp -> Entity -> FileDraft
generateEntityState wasp entity
    = createSimpleEntityFileDraft wasp entity (entityStatePathInSrc entity) "state.js"

generateEntityActionTypes :: Wasp -> Entity -> FileDraft
generateEntityActionTypes wasp entity
    = createSimpleEntityFileDraft wasp entity (entityActionTypesPathInSrc entity)
                                  "actionTypes.js"

generateEntityActions :: Wasp -> Entity -> FileDraft
generateEntityActions wasp entity
    = createSimpleEntityFileDraft wasp entity (entityActionsPathInSrc entity) "actions.js"

generateEntityComponents :: Wasp -> Entity -> [FileDraft]
generateEntityComponents wasp entity =
    [ generateEntityCreateForm wasp entity
    , generateEntityList wasp entity
    ]

-- TODO: add tests / update tests.
-- TODO: I need to pass more complex data here, so that I can build field inputs from it
--   in mustache template. To do this, since musatche is logicless, I need to pass template data like
--   typedFields: [{ stringField: {...fieldData} }, { booleanField: {...fieldData} }, ...]
--   and then have this in mustache:
--   {=# typedFields =}
--   {=# stringField =}
--     ... Code when field is string. ...
--   {=/ stringField =}
--   {=# booleanField =}
--     ... Code when field is boolean. ...
--   {=/ booleanField =}
--   {=/ typedFields =}
generateEntityCreateForm :: Wasp -> Entity -> FileDraft
generateEntityCreateForm wasp entity
    = createSimpleEntityFileDraft wasp entity (entityCreateFormPathInSrc entity)
                                  ("components" </> "CreateForm.js")

-- TODO(matija): do I need wasp at all?
-- | Generates list component for the specified entity, so user can see all the
-- entity instances.
generateEntityList :: Wasp -> Entity -> FileDraft
generateEntityList wasp entity
    = createSimpleEntityFileDraft wasp entity (entityListPathInSrc entity)
                                  ("components" </> "List.js")

-- | Helper function that captures common logic for generating entity file draft.
createSimpleEntityFileDraft :: Wasp -> Entity -> FilePath -> FilePath -> FileDraft
createSimpleEntityFileDraft wasp entity dstPathInSrc srcPathInEntityTemplatesDir
    = createTemplateFileDraft dstPath srcPath templateData
  where
    srcPath = entityTemplatesDirPath </> srcPathInEntityTemplatesDir
    dstPath = "src" </> dstPathInSrc
    templateData = entityTemplateData wasp entity

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

-- | Location in templates where entity related templates reside.
entityTemplatesDirPath :: FilePath
entityTemplatesDirPath = "src" </> "entities" </> "_entity"


-- * Paths of generated code (relative to src/ directory)

entityDirPathInSrc :: Entity -> FilePath
entityDirPathInSrc entity = "entities" </> Util.camelToKebabCase (entityName entity)

entityStatePathInSrc :: Entity -> FilePath
entityStatePathInSrc entity = (entityDirPathInSrc entity) </> "state.js"

entityActionsPathInSrc :: Entity -> FilePath
entityActionsPathInSrc entity = (entityDirPathInSrc entity) </> "actions.js"

entityActionTypesPathInSrc :: Entity -> FilePath
entityActionTypesPathInSrc entity = (entityDirPathInSrc entity) </> "actionTypes.js"

entityClassPathInSrc :: Entity -> FilePath
entityClassPathInSrc entity = (entityDirPathInSrc entity) </> (entityName entity) <.> "js"

-- * Components

entityComponentsDirPathInSrc :: Entity -> FilePath
entityComponentsDirPathInSrc entity = (entityDirPathInSrc entity) </> "components"

entityCreateFormPathInSrc :: Entity -> FilePath
entityCreateFormPathInSrc entity = (entityComponentsDirPathInSrc entity) </> "CreateForm.js"

entityListPathInSrc :: Entity -> FilePath
entityListPathInSrc entity = (entityComponentsDirPathInSrc entity) </> "List.js"
