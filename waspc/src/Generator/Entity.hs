module Generator.Entity
       ( generateEntities

       , entityDirPathInSrc
       , entityClassPathInSrc
       , entityStatePathInSrc
       , entityActionsPathInSrc

       -- EXPORTED FOR TESTING:
       , generateEntityClass
       , generateEntityState
       , generateEntityActions
       , generateEntityActionTypes
       , generateEntityCreateForm

       , entityTemplatesDirPath
       ) where

import Data.Maybe (fromJust)
import Path ((</>), relfile)
import qualified Path
import qualified Path.Aliases as Path

import Wasp
import Generator.FileDraft
import qualified Generator.Common as Common
import Generator.Entity.EntityForm (generateEntityCreateForm)
import Generator.Entity.EntityList (generateEntityList)
import Generator.Entity.Common
    ( entityTemplatesDirPath
    , entityTemplateData
    , entityDirPathInSrc
    )


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
    = createSimpleEntityFileDraft wasp entity (entityClassPathInSrc entity) [relfile|_Entity.js|]

generateEntityState :: Wasp -> Entity -> FileDraft
generateEntityState wasp entity
    = createSimpleEntityFileDraft wasp entity (entityStatePathInSrc entity) [relfile|state.js|]

generateEntityActionTypes :: Wasp -> Entity -> FileDraft
generateEntityActionTypes wasp entity
    = createSimpleEntityFileDraft wasp entity (entityActionTypesPathInSrc entity) [relfile|actionTypes.js|]

generateEntityActions :: Wasp -> Entity -> FileDraft
generateEntityActions wasp entity
    = createSimpleEntityFileDraft wasp entity (entityActionsPathInSrc entity) [relfile|actions.js|]

generateEntityComponents :: Wasp -> Entity -> [FileDraft]
generateEntityComponents wasp entity = concat
    [ generateEntityCreateForms wasp entity
    , generateEntityLists wasp entity
    ]

-- | Generates creation forms for the given entity.
generateEntityCreateForms :: Wasp -> Entity -> [FileDraft]
generateEntityCreateForms wasp entity = map (generateEntityCreateForm wasp) entityForms
    where
        entityForms = getEntityFormsForEntity wasp entity

-- | Generates list components for the given entity.
generateEntityLists :: Wasp -> Entity -> [FileDraft]
generateEntityLists wasp entity = map (generateEntityList wasp) entityLists
    where
        entityLists = getEntityListsForEntity wasp entity

-- | Helper function that captures common logic for generating entity file draft.
createSimpleEntityFileDraft :: Wasp -> Entity -> Path.RelFile -> Path.RelFile -> FileDraft
createSimpleEntityFileDraft wasp entity dstPathInSrc srcPathInEntityTemplatesDir
    = createTemplateFileDraft dstPath srcPath (Just templateData)
  where
    srcPath = entityTemplatesDirPath </> srcPathInEntityTemplatesDir
    dstPath = Common.srcDirPath </> dstPathInSrc
    templateData = entityTemplateData wasp entity

-- * Paths of generated code (relative to src/ directory)

entityStatePathInSrc :: Entity -> Path.RelFile
entityStatePathInSrc entity = entityDirPathInSrc entity </> [relfile|state.js|]

entityActionsPathInSrc :: Entity -> Path.RelFile
entityActionsPathInSrc entity = entityDirPathInSrc entity </> [relfile|actions.js|]

entityActionTypesPathInSrc :: Entity -> Path.RelFile
entityActionTypesPathInSrc entity = entityDirPathInSrc entity </> [relfile|actionTypes.js|]

entityClassPathInSrc :: Entity -> Path.RelFile
entityClassPathInSrc entity = entityDirPathInSrc entity </>
                              (fromJust $ Path.parseRelFile $ (entityName entity) ++ ".js")
