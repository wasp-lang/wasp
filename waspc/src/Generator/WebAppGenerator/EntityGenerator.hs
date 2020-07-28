module Generator.WebAppGenerator.EntityGenerator
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
import qualified Path as P
import Util (jsonSet)

import StrongPath (Path, Rel, File, (</>))
import qualified StrongPath as SP
import Wasp
import qualified Wasp.Action
import Generator.FileDraft
import qualified Generator.WebAppGenerator.Common as Common
import Generator.WebAppGenerator.EntityGenerator.EntityFormGenerator (generateEntityCreateForm)
import Generator.WebAppGenerator.EntityGenerator.EntityListGenerator (generateEntityList)
import Generator.WebAppGenerator.EntityGenerator.Common
    ( entityTemplatesDirPath
    , entityTemplateData
    , entityDirPathInSrc
    , asEntityTmplFile
    , EntityTemplatesDir
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
generateEntityClass wasp entity = createSimpleEntityFileDraft wasp entity dstPath tmplPath
    where dstPath = entityClassPathInSrc entity
          tmplPath = asEntityTmplFile [P.relfile|_Entity.js|]

generateEntityState :: Wasp -> Entity -> FileDraft
generateEntityState wasp entity = createSimpleEntityFileDraft wasp entity dstPath tmplPath
  where dstPath = entityStatePathInSrc entity
        tmplPath = asEntityTmplFile [P.relfile|state.js|]

generateEntityActionTypes :: Wasp -> Entity -> FileDraft
generateEntityActionTypes wasp entity = createSimpleEntityFileDraft wasp entity dstPath tmplPath
  where dstPath = entityActionTypesPathInSrc entity
        tmplPath = asEntityTmplFile [P.relfile|actionTypes.js|]

generateEntityActions :: Wasp -> Entity -> FileDraft
generateEntityActions wasp entity = createEntityFileDraft dstPath tmplPath (Just templateData)
  where
    dstPath = entityActionsPathInSrc entity
    tmplPath = asEntityTmplFile [P.relfile|actions.js|]
    templateData = jsonSet "entityActions" (Aeson.toJSON entityActions) (entityTemplateData wasp entity)
    entityActions = getActionsForEntity wasp entity

-- | Provides information on how to import and use given action.
-- Returns: (path to import action from, identifier under which it is exported).
-- NOTE: This function is in this module because this is where logic for generating action is,
--   but ideally that would move to more-standalone action generator and so would this function.
getImportInfoForAction :: Wasp -> Wasp.Action.Action -> (Path (Rel Common.WebAppSrcDir) File, String)
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
createSimpleEntityFileDraft :: Wasp
                            -> Entity
                            -> Path (Rel Common.WebAppSrcDir) File
                            -> Path (Rel EntityTemplatesDir) File
                            -> FileDraft
createSimpleEntityFileDraft wasp entity dstPathInSrc srcPathInEntityTemplatesDir
    = createEntityFileDraft dstPathInSrc srcPathInEntityTemplatesDir (Just templateData)
  where
    templateData = entityTemplateData wasp entity

createEntityFileDraft :: Path (Rel Common.WebAppSrcDir) File
                      -> Path (Rel EntityTemplatesDir) File
                      -> Maybe Aeson.Value -> FileDraft
createEntityFileDraft dstPathInSrc srcPathInEntityTemplatesDir maybeTemplateData =
    createTemplateFileDraft dstPath srcPath maybeTemplateData
  where
    srcPath = entityTemplatesDirPath </> srcPathInEntityTemplatesDir
    dstPath = Common.webAppSrcDirInProjectRootDir </> dstPathInSrc

-- * Paths of generated code.

entityStatePathInSrc :: Entity -> Path (Rel Common.WebAppSrcDir) File
entityStatePathInSrc entity = entityDirPathInSrc entity </> SP.fromPathRelFile [P.relfile|state.js|]

entityActionsPathInSrc :: Entity -> Path (Rel Common.WebAppSrcDir) File
entityActionsPathInSrc entity = entityDirPathInSrc entity </> SP.fromPathRelFile [P.relfile|actions.js|]

entityActionTypesPathInSrc :: Entity -> Path (Rel Common.WebAppSrcDir) File
entityActionTypesPathInSrc entity = entityDirPathInSrc entity </> SP.fromPathRelFile [P.relfile|actionTypes.js|]

entityClassPathInSrc :: Entity -> Path (Rel Common.WebAppSrcDir) File
entityClassPathInSrc entity = entityDirPathInSrc entity </>
                              (fromJust $ SP.parseRelFile $ (entityName entity) ++ ".js")
