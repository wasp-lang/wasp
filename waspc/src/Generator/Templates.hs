module Generator.Templates
  ( getTemplatesDirAbsPath,
    getTemplateFileAbsPath,
    compileAndRenderTemplate,
    TemplatesDir,
  )
where

import qualified Data
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import StrongPath (Abs, Dir, File', Path', Rel, reldir, (</>))
import qualified StrongPath as SP
import qualified Text.Mustache as Mustache
import Text.Mustache.Render (SubstitutionError (..))
import Text.Printf (printf)

-- TODO: Write tests for this file! But first we need to decouple logic from IO
--   so that we can mock it.

data TemplatesDir

-- | Returns absolute path of templates root directory.
getTemplatesDirAbsPath :: IO (Path' Abs (Dir TemplatesDir))
getTemplatesDirAbsPath = (</> templatesDirPathInDataDir) <$> Data.getAbsDataDirPath

-- | Takes template file path relative to templates root directory and returns
--   its absolute path.
getTemplateFileAbsPath :: Path' (Rel TemplatesDir) File' -> IO (Path' Abs File')
getTemplateFileAbsPath relTmplFilePath = (</> relTmplFilePath) <$> getTemplatesDirAbsPath

templatesDirPathInDataDir :: Path' (Rel Data.DataDir) (Dir TemplatesDir)
templatesDirPathInDataDir = [reldir|Generator/templates|]

compileAndRenderTemplate ::
  -- | Path to the template file.
  Path' (Rel TemplatesDir) File' ->
  -- | JSON to be provided as template data.
  Aeson.Value ->
  IO Text
compileAndRenderTemplate relTmplPath tmplData = do
  mustacheTemplate <- compileMustacheTemplate relTmplPath
  renderMustacheTemplate mustacheTemplate tmplData

compileMustacheTemplate ::
  -- | Path to the template file.
  Path' (Rel TemplatesDir) File' ->
  IO Mustache.Template
compileMustacheTemplate relTmplPath = do
  templatesDirAbsPath <- getTemplatesDirAbsPath
  absTmplPath <- getTemplateFileAbsPath relTmplPath
  eitherTemplate <-
    Mustache.automaticCompile
      [SP.fromAbsDir templatesDirAbsPath]
      (SP.fromAbsFile absTmplPath)
  return $ either raiseCompileError id eitherTemplate
  where
    raiseCompileError err =
      error $ -- TODO: Handle these errors better?
        printf "Compilation of template %s failed. %s" (show relTmplPath) (show err)

areAllErrorsSectionDataNotFound :: [SubstitutionError] -> Bool
areAllErrorsSectionDataNotFound = all isSectionDataNotFoundError
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
  if null errors || areAllErrorsSectionDataNotFound errors
    then return fileText
    else error $ "Unexpected errors occured while rendering template: " ++ show errors
