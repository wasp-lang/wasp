{-# LANGUAGE OverloadedStrings #-}
module Generator.PageGenerator
       ( generatePages

       -- EXPORTED ONLY FOR TESTS:
       , generatePage
       ) where

import Data.Aeson ((.=), object)
import System.FilePath ((</>), (<.>))

import qualified Util
import Wasp
import Generator.FileDraft
import qualified Generator.EntityGenerator as EntityGenerator


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
        , "entityLowerName" .= (Util.toLowerFirst $ entityName entity)
        , "entityUpperName" .= (Util.toUpperFirst $ entityName entity)
        , "entityStatePath" .= ("./" ++ (EntityGenerator.entityStatePathInSrc entity))
        , "entityActionsPath" .= ("./" ++ (EntityGenerator.entityActionsPathInSrc entity))
        , "entityClassPath" .= ("./" ++ (EntityGenerator.entityClassPathInSrc entity))
        ]
