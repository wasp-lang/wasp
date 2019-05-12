{-# LANGUAGE OverloadedStrings #-}
module Generator.Generators
       ( generateWebApp

       -- EXPORTED ONLY FOR TESTS:
       , generatePage
       ) where

import Data.Aeson ((.=), object, ToJSON(..))
import System.FilePath (FilePath, (</>), (<.>))

import Generator.FileDraft
import Wasp


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

generateSrcDir :: Wasp -> [FileDraft]
generateSrcDir wasp
    = (createCopyFileDraft ("src" </> "logo.png") ("src" </> "logo.png"))
    : map (\path -> simpleTemplateFileDraft ("src/" </> path) wasp)
        [ "App.js"
        , "App.test.js"
        , "App.css"
        , "index.js"
        , "index.css"
        , "reducers.js"
        , "router.js"
        , "serviceWorker.js"
        , "store/index.js"
        , "store/middleware/logger.js"
        ]
    ++ generatePages wasp

generatePages :: Wasp -> [FileDraft]
generatePages wasp = generatePage wasp <$> getPages wasp

generatePage :: Wasp -> Page -> FileDraft
generatePage wasp page = createTemplateFileDraft dstPath srcPath templateData
  where
    srcPath = "src" </> "_Page.js"
    dstPath = "src" </> (pageName page) <.> "js"
    templateData = object ["wasp" .= wasp, "page" .= page]


-- | Creates template file draft that uses given path as both src and dst path
--   and wasp as template data.
simpleTemplateFileDraft :: FilePath -> Wasp -> FileDraft
simpleTemplateFileDraft path wasp = createTemplateFileDraft path path (toJSON wasp)
