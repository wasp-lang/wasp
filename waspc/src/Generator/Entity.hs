module Generator.Entity
       ( generateEntities

       , entityDirPathInSrc
       , entityClassPathInSrc
       , entityStatePathInSrc
       , entityActionsPathInSrc

       , getImportInfoForAction

       -- EXPORTED FOR TESTING:
       , generateEntityClass
       , generateEntityState
       , generateEntityActions
       , generateEntityActionTypes
       , generateEntityCreateForm

       , entityTemplatesDirPath
       ) where

import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust)
import Path ((</>), relfile)
import qualified Path
import qualified Path.Aliases as Path
import Util (jsonSet)

import Wasp
import qualified Wasp.Action
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
    = createEntityFileDraft (entityActionsPathInSrc entity) [relfile|actions.js|] (Just templateData)
  where
    entityActions = getActionsForEntity wasp entity
    templateData = jsonSet "entityActions" (Aeson.toJSON entityActions) (entityTemplateData wasp entity)

-- | Provides information on how to import and use given action.
-- Returns: (path (in src dir) to import action from, identifier under which it is exported).
-- NOTE: This function is in this module because this is where logic for generating action is,
--   but ideally that would move to more-standalone action generator and so would this function.
getImportInfoForAction :: Wasp -> Wasp.Action.Action -> (Path.RelFile, String)
getImportInfoForAction wasp action = (pathInSrc, exportedIdentifier)
  where
    -- NOTE: For now here we bravely assume that entity with such name exists.
    Just entity = Wasp.getEntityByName wasp $ Wasp.Action._entityName action
    pathInSrc = entityActionsPathInSrc entity
    exportedIdentifier = Wasp.Action._name action


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
    = createEntityFileDraft dstPathInSrc srcPathInEntityTemplatesDir (Just templateData)
  where
    templateData = entityTemplateData wasp entity

createEntityFileDraft :: Path.RelFile -> Path.RelFile -> Maybe Aeson.Value -> FileDraft
createEntityFileDraft dstPathInSrc srcPathInEntityTemplatesDir maybeTemplateData =
    createTemplateFileDraft dstPath srcPath maybeTemplateData
  where
    srcPath = entityTemplatesDirPath </> srcPathInEntityTemplatesDir
    dstPath = Common.srcDirPath </> dstPathInSrc

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
