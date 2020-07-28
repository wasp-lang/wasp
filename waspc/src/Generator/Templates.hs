module Generator.Templates
       ( getTemplatesDirAbsPath
       , getTemplateFileAbsPath
       , compileAndRenderTemplate
       , DataDir, TemplatesDir
       ) where

import qualified Text.Mustache as Mustache
import Text.Mustache.Render (SubstitutionError(..))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Text.Printf (printf)
import qualified Path as P

import StrongPath (Path, File, Dir, Abs, Rel, (</>))
import qualified StrongPath as SP


import qualified Paths_waspc

-- TODO: Write tests for this file! But first we need to decouple logic from IO
--   so that we can mock it.

data DataDir
data TemplatesDir

-- | Returns absolute path of templates root directory.
getTemplatesDirAbsPath :: IO (Path Abs (Dir TemplatesDir))
getTemplatesDirAbsPath = do
    dataDir <- getAbsDataDirPath
    return $ dataDir </> templatesDirPathInDataDir

-- | Takes template file path relative to templates root directory and returns
--   its absolute path.
getTemplateFileAbsPath :: Path (Rel TemplatesDir) File -> IO (Path Abs File)
getTemplateFileAbsPath tmplFilePathInTemplatesDir = absPathOfTemplateFileInDataDir tmplFilePathInDataDir
  where
    tmplFilePathInDataDir :: Path (Rel DataDir) File
    tmplFilePathInDataDir = templatesDirPathInDataDir </> tmplFilePathInTemplatesDir

    absPathOfTemplateFileInDataDir :: Path (Rel DataDir) File -> IO (Path Abs File)
    absPathOfTemplateFileInDataDir filePath =
        (Paths_waspc.getDataFileName $ SP.toFilePath filePath) >>= SP.parseAbsFile

templatesDirPathInDataDir :: Path (Rel DataDir) (Dir TemplatesDir)
templatesDirPathInDataDir = SP.fromPathRelDir [P.reldir|Generator/templates|]

getAbsDataDirPath :: IO (Path Abs (Dir DataDir))
getAbsDataDirPath = Paths_waspc.getDataDir >>= SP.parseAbsDir

compileAndRenderTemplate
    :: Path (Rel TemplatesDir) File  -- ^ Path to the template file.
    -> Aeson.Value  -- ^ JSON to be provided as template data.
    -> IO Text
compileAndRenderTemplate relTmplPath tmplData = do
    mustacheTemplate <- compileMustacheTemplate relTmplPath
    renderMustacheTemplate mustacheTemplate tmplData

compileMustacheTemplate
    :: Path (Rel TemplatesDir) File  -- ^ Path to the template file.
    -> IO Mustache.Template
compileMustacheTemplate relTmplPath = do
    templatesDirAbsPath <- getTemplatesDirAbsPath
    absTmplPath <- getTemplateFileAbsPath relTmplPath
    eitherTemplate <- Mustache.automaticCompile [SP.toFilePath templatesDirAbsPath]
                                                (SP.toFilePath absTmplPath)
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
