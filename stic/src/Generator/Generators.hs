{-# LANGUAGE OverloadedStrings #-}
module Generator.Generators
       ( generateWebApp

       -- EXPORTED ONLY FOR TESTS:
       , generatePage
       ) where

import Data.Aeson ((.=), object, ToJSON(..))
import Data.Char (toLower, toUpper)
import System.FilePath (FilePath, (</>), (<.>))

import Generator.FileDraft
import Wasp
import qualified Util


generateWebApp :: Wasp -> [FileDraft]
generateWebApp wasp = concatMap ($ wasp)
    [ generateReadme
    , generatePackageJson
    , generateGitignore
    , generatePublicDir
    , generateSrcDir
    ]

generateReadme :: Wasp -> [FileDraft]
generateReadme wasp = [simpleTemplateFileDraft "README.md" wasp]

generatePackageJson :: Wasp -> [FileDraft]
generatePackageJson wasp = [simpleTemplateFileDraft "package.json" wasp]

generateGitignore :: Wasp -> [FileDraft]
generateGitignore wasp = [createTemplateFileDraft ".gitignore" "gitignore" (toJSON wasp)]

generatePublicDir :: Wasp -> [FileDraft]
generatePublicDir wasp
    = createCopyFileDraft ("public" </> "favicon.ico") ("public" </> "favicon.ico")
    : map (\path -> simpleTemplateFileDraft ("public/" </> path) wasp)
        [ "index.html"
        , "manifest.json"
        ]

-- * Src dir

generateSrcDir :: Wasp -> [FileDraft]
generateSrcDir wasp
    = (createCopyFileDraft ("src" </> "logo.png") ("src" </> "logo.png"))
    : map (\path -> simpleTemplateFileDraft ("src/" </> path) wasp)
        [ "App.js"
        , "App.test.js"
        , "App.css"
        , "index.js"
        , "index.css"
        , "router.js"
        , "serviceWorker.js"
        , "store/index.js"
        , "store/middleware/logger.js"
        ]
    ++ generatePages wasp
    ++ generateEntities wasp
    ++ [generateReducersJs wasp]

generateReducersJs :: Wasp -> FileDraft
generateReducersJs wasp = createTemplateFileDraft dstPath srcPath templateData
  where
    srcPath = "src" </> "reducers.js"
    dstPath = srcPath
    templateData = object
        [ "wasp" .= wasp
        , "entities" .= map toEntityData (getEntities wasp)
        ]
    toEntityData entity = object
        [ "entity" .= entity
        , "entityLowerName" .= entityLowerName entity
        , "entityStatePath" .= ("./" ++ (entityStatePath entity))
        ]

-- * Pages

generatePages :: Wasp -> [FileDraft]
generatePages wasp = generatePage wasp <$> getPages wasp

generatePage :: Wasp -> Page -> FileDraft
generatePage wasp page = createTemplateFileDraft dstPath srcPath templateData
  where
    srcPath = "src" </> "_Page.js"
    dstPath = "src" </> (pageName page) <.> "js"
    templateData = object
        [ "wasp" .= wasp
        , "page" .= page
        , "entities" .= map toEntityData (getEntities wasp)
        ]
    toEntityData entity = object
        [ "entity" .= entity
        , "entityLowerName" .= entityLowerName entity
        , "entityUpperName" .= entityUpperName entity
        , "entityStatePath" .= ("./" ++ (entityStatePath entity))
        , "entityActionsPath" .= ("./" ++ (entityActionsPath entity))
        , "entityClassPath" .= ("./" ++ (entityClassPath entity))
        ]

-- * Entities

generateEntities :: Wasp -> [FileDraft]
generateEntities wasp = concat $ generateEntity wasp <$> getEntities wasp

-- TODO(martin): Create EntityData object that will contain more stuff,
--   like small camel case name and similar, that will be representation used in the
--   template files, instead of Entity directly.
--   Then build that from Entity and pass that to templates.
--   I could even have one data type per template.
--   Or, have function that builds json?

-- TODO(martin): Also, I should extract entity stuff into separate module,
--   there is too much logic here already.

-- TODO(martin): This file has lot of duplication + is missing tests, work on that.

generateEntity :: Wasp -> Entity -> [FileDraft]
generateEntity wasp entity =
    [ generateEntityClass wasp entity
    , generateEntityState wasp entity
    , generateEntityActionTypes wasp entity
    , generateEntityActions wasp entity
    ]

generateEntityClass :: Wasp -> Entity -> FileDraft
generateEntityClass wasp entity = createTemplateFileDraft dstPath srcPath templateData
  where
    srcPath = "src" </> "entities" </> "_entity" </> "_Entity.js"
    dstPath = "src" </> entityClassPath entity
    templateData = object
        [ "wasp" .= wasp
        , "entity" .= entity
        ]

generateEntityState :: Wasp -> Entity -> FileDraft
generateEntityState wasp entity = createTemplateFileDraft dstPath srcPath templateData
  where
    srcPath = "src" </> "entities" </> "_entity" </> "state.js"
    dstPath = "src" </> entityStatePath entity
    templateData = object
        [ "wasp" .= wasp
        , "entity" .= entity
        ]

generateEntityActionTypes :: Wasp -> Entity -> FileDraft
generateEntityActionTypes wasp entity = createTemplateFileDraft dstPath srcPath templateData
  where
    srcPath = "src" </> "entities" </> "_entity" </> "actionTypes.js"
    dstPath = "src" </> "entities" </> (entityDirName entity) </> "actionTypes.js"
    templateData = object
        [ "wasp" .= wasp
        , "entity" .= entity
        , "entityLowerName" .= entityLowerName entity
        ]

generateEntityActions :: Wasp -> Entity -> FileDraft
generateEntityActions wasp entity = createTemplateFileDraft dstPath srcPath templateData
  where
    srcPath = "src" </> "entities" </> "_entity" </> "actions.js"
    dstPath = "src" </> entityActionsPath entity
    templateData = object
        [ "wasp" .= wasp
        , "entity" .= entity
        , "entityLowerName" .= entityLowerName entity
        ]

entityDirName :: Entity -> String
entityDirName entity = Util.camelToKebabCase (entityName entity)

entityLowerName :: Entity -> String
entityLowerName Entity{entityName=name} = (toLower $ head name) : (tail name)

entityUpperName :: Entity -> String
entityUpperName Entity{entityName=name} = (toUpper $ head name) : (tail name)

entityStatePath :: Entity -> String
entityStatePath entity = "entities" </> (entityDirName entity) </> "state.js"

entityActionsPath :: Entity -> String
entityActionsPath entity = "entities" </> (entityDirName entity) </> "actions.js"

entityClassPath :: Entity -> String
entityClassPath entity = "entities" </> (entityDirName entity) </> (entityName entity) <.> "js"

-- * Helpers

-- | Creates template file draft that uses given path as both src and dst path
--   and wasp as template data.
simpleTemplateFileDraft :: FilePath -> Wasp -> FileDraft
simpleTemplateFileDraft path wasp = createTemplateFileDraft path path (toJSON wasp)
