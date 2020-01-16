module Generator.Entity
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
       , generateEntityCreateForm

       , entityTemplatesDirPath
       ) where

import System.FilePath (FilePath, (</>), (<.>))

import Wasp
import Generator.FileDraft
import qualified Generator.Common as Common
import Generator.Entity.EntityForm
import Generator.Entity.Common


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

-- TODO(matija): currently we are generating these components automatically, as soon as the
-- entity is defined. Now we are changing this and will generate them on-demand, depending on
-- what is in Wasp.
generateEntityComponents :: Wasp -> Entity -> [FileDraft]
generateEntityComponents wasp entity = concat
    [ generateEntityCreateForms wasp entity

    -- TODO(matija): this will become listS as well (in the future PR).
    , [generateEntityList wasp entity]
    ]

-- | Generates creation forms for the given entity.
generateEntityCreateForms :: Wasp -> Entity -> [FileDraft]
generateEntityCreateForms wasp entity =
    map (generateEntityCreateForm wasp) entityForms
    where
        entityForms = getEntityFormsForEntity wasp entity

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
    = createTemplateFileDraft dstPath srcPath (Just templateData)
  where
    srcPath = entityTemplatesDirPath </> srcPathInEntityTemplatesDir
    dstPath = Common.srcDirPath </> dstPathInSrc
    templateData = entityTemplateData wasp entity

-- * Paths of generated code (relative to src/ directory)

entityStatePathInSrc :: Entity -> FilePath
entityStatePathInSrc entity = (entityDirPathInSrc entity) </> "state.js"

entityActionsPathInSrc :: Entity -> FilePath
entityActionsPathInSrc entity = (entityDirPathInSrc entity) </> "actions.js"

entityActionTypesPathInSrc :: Entity -> FilePath
entityActionTypesPathInSrc entity = (entityDirPathInSrc entity) </> "actionTypes.js"

entityClassPathInSrc :: Entity -> FilePath
entityClassPathInSrc entity = (entityDirPathInSrc entity) </> (entityName entity) <.> "js"

-- * Components

entityListPathInSrc :: Entity -> FilePath
entityListPathInSrc entity = (entityComponentsDirPathInSrc entity) </> "List.js"
