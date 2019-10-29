module Generator.PageGenerator
       ( generatePages

       -- EXPORTED ONLY FOR TESTS:
       , generatePage
       , generatePageComponent
       , generatePageStyle
       ) where

import Data.Aeson ((.=), object)
import System.FilePath (FilePath, (</>), (<.>))

import qualified Util
import Wasp
import Generator.FileDraft
import qualified Generator.EntityGenerator as EntityGenerator


generatePages :: Wasp -> [FileDraft]
generatePages wasp = concatMap (generatePage wasp) (getPages wasp)

generatePage :: Wasp -> Page -> [FileDraft]
generatePage wasp page =
    [ generatePageComponent wasp page
    ]
    ++ generatePageStyle wasp page

generatePageComponent :: Wasp -> Page -> FileDraft
generatePageComponent wasp page = createTemplateFileDraft dstPath srcPath templateData
  where
    srcPath = "src" </> "_Page.js"
    dstPath = "src" </> (pageName page) <.> "js"
    templateData = object $
        [ "wasp" .= wasp
        , "page" .= page
        , "entities" .= map toEntityData (getEntities wasp)
        ]
        ++ maybe []
                 (\_ -> ["pageStylePath" .= ("." </> pageStylePathInSrcDir page)])
                 (pageStyle page)
    toEntityData entity = object
        [ "entity" .= entity
        , "entityLowerName" .= (Util.toLowerFirst $ entityName entity)
        , "entityUpperName" .= (Util.toUpperFirst $ entityName entity)
        , "entityStatePath" .= ( "." </> EntityGenerator.entityStatePathInSrc entity)
        , "entityActionsPath" .= ("." </> EntityGenerator.entityActionsPathInSrc entity)
        , "entityClassPath" .= ("." </> EntityGenerator.entityClassPathInSrc entity)
        , "entityCreateFormPath" .= ("." </> EntityGenerator.entityCreateFormPathInSrc entity)
        , "entityListPath" .= ("." </> EntityGenerator.entityListPathInSrc entity)
        ]

-- TODO(martin): write test.
generatePageStyle :: Wasp -> Page -> [FileDraft]
generatePageStyle _ page = maybe
    []
    (\style -> [createTextFileDraft dstPath style])
    (pageStyle page)
  where
    dstPath = "src" </> pageStylePathInSrcDir page

pageStylePathInSrcDir :: Page -> FilePath
pageStylePathInSrcDir page = (pageName page) <.> "css"
