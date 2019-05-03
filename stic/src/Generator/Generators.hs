{-# LANGUAGE OverloadedStrings #-}
module Generator.Generators
       ( generateWebApp
       ) where

import qualified Data.Aeson as Aeson
import System.FilePath (FilePath, (</>))

import Generator.FileDraft
import Wasp


defaultCreateTemplateFileDraft :: FilePath -> Wasp -> FileDraft
defaultCreateTemplateFileDraft path wasp = createTemplateFileDraft path path (Aeson.toJSON wasp)


type FileDraftGenerator = Wasp -> [FileDraft]

generateWebApp :: FileDraftGenerator
generateWebApp wasp = concat $ map ($ wasp)
    [ generateReadme
    , generatePackageJson
    , generateGitignore
    , generatePublicDir
    , generateSrcDir
    ]

generateReadme :: FileDraftGenerator
generateReadme wasp = [defaultCreateTemplateFileDraft "README.md" wasp]

generatePackageJson :: FileDraftGenerator
generatePackageJson wasp = [defaultCreateTemplateFileDraft "package.json" wasp]

generateGitignore :: FileDraftGenerator
generateGitignore wasp = [createTemplateFileDraft ".gitignore" "gitignore" (Aeson.toJSON wasp)]

generatePublicDir :: FileDraftGenerator
generatePublicDir wasp
    = createCopyFileDraft ("public" </> "favicon.ico") ("public" </> "favicon.ico")
    : map (\path -> defaultCreateTemplateFileDraft ("public/" </> path) wasp)
        [ "index.html"
        , "manifest.json"
        ]

generateSrcDir :: FileDraftGenerator
generateSrcDir wasp
    = (createCopyFileDraft ("src" </> "logo.png") ("src" </> "logo.png"))
    : map (\path -> defaultCreateTemplateFileDraft ("src/" </> path) wasp)
        [ "App.css"
        , "App.js"
        , "App.test.js"
        , "index.css"
        , "index.js"
        , "serviceWorker.js"
        ]
