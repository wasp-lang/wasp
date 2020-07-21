module Generator.Templates
       ( getTemplatesDirAbsPath
       , getTemplateFileAbsPath
       , compileAndRenderTemplate
       ) where

import qualified Text.Mustache as Mustache
import Text.Mustache.Render (SubstitutionError(..))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Text.Printf (printf)
import Path ((</>), reldir)
import qualified Path
import qualified Path.Aliases as Path

import qualified Paths_waspc

-- TODO: Write tests for this file! But first we need to decouple logic from IO
--   so that we can mock it.

-- | Returns absolute path of templates root directory.
getTemplatesDirAbsPath :: IO Path.AbsDir
getTemplatesDirAbsPath = do
    absDataDirPath <- Paths_waspc.getDataDir >>= Path.parseAbsDir
    return $ absDataDirPath </> templatesDirPathInDataDir

-- | Takes template file path relative to templates root directory and returns
--   its absolute path.
getTemplateFileAbsPath :: Path.RelFile -> IO Path.AbsFile
getTemplateFileAbsPath tmplFilePathInTemplatesDir =
    Paths_waspc.getDataFileName (Path.toFilePath tmplFilePathInDataDir) >>= Path.parseAbsFile
  where
    tmplFilePathInDataDir = templatesDirPathInDataDir </> tmplFilePathInTemplatesDir

templatesDirPathInDataDir :: Path.RelDir
templatesDirPathInDataDir = [reldir|Generator|] </> [reldir|templates|]

compileAndRenderTemplate
    :: Path.RelFile  -- ^ Path to the template file, relative to templates root dir.
    -> Aeson.Value  -- ^ JSON to be provided as template data.
    -> IO Text
compileAndRenderTemplate relTmplPath tmplData = do
    mustacheTemplate <- compileMustacheTemplate relTmplPath
    renderMustacheTemplate mustacheTemplate tmplData

compileMustacheTemplate
    :: Path.RelFile  -- ^ Path to the template file, relative to templates root dir.
    -> IO Mustache.Template
compileMustacheTemplate relTmplPath = do
    templatesDirAbsPath <- getTemplatesDirAbsPath
    absTmplPath <- getTemplateFileAbsPath relTmplPath
    eitherTemplate <- Mustache.automaticCompile [Path.toFilePath templatesDirAbsPath] (Path.toFilePath absTmplPath)
    return $ either raiseCompileError id eitherTemplate
  where
    raiseCompileError err = error $  -- TODO: Handle these errors better?
        printf "Compilation of template %s failed. %s" (show relTmplPath) (show err)

areAllErrorsSectionDataNotFound :: [SubstitutionError] -> Bool
areAllErrorsSectionDataNotFound subsErrors = all isSectionDataNotFoundError subsErrors
  where
    isSectionDataNotFoundError e = case e of
        SectionTargetNotFound _ -> True
        _ -> False

renderMustacheTemplate :: Mustache.Template -> Aeson.Value -> IO Text
renderMustacheTemplate mustacheTemplate templateData = do
    let mustacheTemplateData = Mustache.toMustache templateData
    let (errors, fileText) =
            Mustache.checkedSubstituteValue mustacheTemplate mustacheTemplateData

    -- NOTE(matija): Mustache reports errors when object does
    -- not have a property specified in the template, which we use to implement
    -- conditionals. This is why we ignore these errors.
    if (null errors) || (areAllErrorsSectionDataNotFound errors)
        then (return fileText)
        else (error $ "Unexpected errors occured while rendering template: "
            ++ (show errors))
