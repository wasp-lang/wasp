{-# LANGUAGE OverloadedStrings #-}
module Generator.Generators
       ( generateWebApp
       ) where

import qualified Data.Aeson as Aeson
import System.FilePath (FilePath, (</>))

import Generator.FileDraft
import Wasp


-- NOTE(martin): Here I define general transformation of App into JSON that I can then easily use
--   as data for templates, but we will probably want to replace this in the future with the better tailored
--   types that are exact fit for what is neeed (for example one type per template).
instance Aeson.ToJSON App where
    toJSON app = Aeson.object
        [ "name" Aeson..= appName app
        , "title" Aeson..= appTitle app
        ]
instance Aeson.ToJSON Wasp where
    toJSON wasp = Aeson.object
        [ "app" Aeson..= getApp wasp
        ]

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
