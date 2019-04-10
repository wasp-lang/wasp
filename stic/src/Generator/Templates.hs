module Generator.Templates
       ( getTemplatesDirAbsPath
       , getTemplateFileAbsPath
       , compileAndRenderTemplate
       ) where

import qualified Text.Mustache as Mustache
import qualified Data.Aeson as Aeson
import System.FilePath ((</>))
import Data.Text (Text)
import Text.Printf (printf)

import qualified Paths_stic

-- TODO: Write tests for this file! But first we need to decouple logic from IO
--   so that we can mock it.

-- | Returns absolute path of templates root directory.
--   NOTE(Martin): Here I set it to react-app, which might be one lvl too deep
--     and will require some changes in the future, but did not want to
--     overengineer for now.
getTemplatesDirAbsPath :: IO FilePath
getTemplatesDirAbsPath = do
    dataDirPath <- Paths_stic.getDataDir
    return $ dataDirPath </> "Generator/templates/react-app"

-- | Takes template file path relative to templates root directory and returns
--   its absolute path.
getTemplateFileAbsPath :: FilePath -> IO FilePath
getTemplateFileAbsPath relFilepath =
    Paths_stic.getDataFileName ("Generator/templates/react-app" </> relFilepath)

compileAndRenderTemplate
    :: FilePath  -- ^ Path to the template file, relative to template root dir.
    -> Aeson.Value  -- ^ JSON to be provided as template data.
    -> IO Text
compileAndRenderTemplate templateRelFilepath templateData = do
    mustacheTemplate <- compileMustacheTemplate templateRelFilepath
    renderMustacheTemplate mustacheTemplate templateData

compileMustacheTemplate
    :: FilePath  -- ^ Path to the template file, relative to template root dir.
    -> IO Mustache.Template
compileMustacheTemplate templateRelPath = do
    templatesDirAbsPath <- getTemplatesDirAbsPath
    templateAbsPath <- getTemplateFileAbsPath templateRelPath
    eitherTemplate <- Mustache.automaticCompile [templatesDirAbsPath] templateAbsPath
    return $ either raiseCompileError id eitherTemplate
  where
    raiseCompileError err = error $  -- TODO: Handle these errors better?
        printf "Compilation of template %s failed. %s" templateRelPath (show err)

renderMustacheTemplate :: Mustache.Template -> Aeson.Value -> IO Text
renderMustacheTemplate mustacheTemplate templateData = do
    let mustacheTemplateData = Mustache.toMustache templateData
    let (errors, fileText) =
            Mustache.checkedSubstituteValue mustacheTemplate mustacheTemplateData
    if (null errors)  -- TODO: Handle these errors better.
        then (return fileText)
        else (error $ "Errors occured while rendering template: " ++ (show errors))
