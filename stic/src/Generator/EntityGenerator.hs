{-# LANGUAGE OverloadedStrings #-}
module Generator.EntityGenerator
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
       , entityTemplatesDirPath
       ) where

import Data.Aeson ((.=), object)
import qualified Data.Aeson as Aeson
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


-- | Helper function that captures common logic for generating entity file draft.
createSimpleEntityFileDraft :: Wasp -> Entity -> FilePath -> FilePath -> FileDraft
createSimpleEntityFileDraft wasp entity dstPathInSrc srcPathInEntityTemplatesDir
    = createTemplateFileDraft dstPath srcPath templateData
  where
    srcPath = entityTemplatesDirPath </> srcPathInEntityTemplatesDir
    dstPath = "src" </> dstPathInSrc
    templateData = entityTemplateData wasp entity

-- | Default generic data for entity templates.
entityTemplateData :: Wasp -> Entity -> Aeson.Value
entityTemplateData wasp entity = object
    [ "wasp" .= wasp
    , "entity" .= entity
    , "entityLowerName" .= (Util.toLowerFirst $ entityName entity)
    ]

-- | Location in templates where entity related templates reside.
entityTemplatesDirPath :: FilePath
entityTemplatesDirPath = "src" </> "entities" </> "_entity"


-- * Paths (relative to src/ directory)

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
