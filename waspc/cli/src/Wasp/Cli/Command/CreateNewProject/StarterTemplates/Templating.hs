module Wasp.Cli.Command.CreateNewProject.StarterTemplates.Templating
  ( replaceTemplatePlaceholdersInTemplateFiles,
  )
where

import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import StrongPath (Abs, Dir, File, Path')
import Wasp.Cli.Command.CreateNewProject.Common (defaultWaspVersionBounds)
import Wasp.Cli.Command.CreateNewProject.ProjectDescription (NewProjectAppName, NewProjectName)
import Wasp.Project.Analyze (findPackageJsonFile, findWaspFile)
import Wasp.Project.Common (WaspProjectDir)
import qualified Wasp.Util.IO as IOUtil

replaceTemplatePlaceholdersInTemplateFiles :: NewProjectAppName -> NewProjectName -> Path' Abs (Dir WaspProjectDir) -> IO ()
replaceTemplatePlaceholdersInTemplateFiles appName projectName projectDir = do
  replaceTemplatePlaceholdersInWaspFile appName projectName projectDir
  replaceTemplatePlaceholdersInPackageJsonFile appName projectName projectDir

-- | Template file for wasp file has placeholders in it that we want to replace
-- in the .wasp file we have written to the disk.
-- If no .wasp file was found in the project, do nothing.
replaceTemplatePlaceholdersInWaspFile ::
  NewProjectAppName -> NewProjectName -> Path' Abs (Dir WaspProjectDir) -> IO ()
replaceTemplatePlaceholdersInWaspFile appName projectName projectDir =
  findWaspFile projectDir >>= \case
    Nothing -> return ()
    Just absMainWaspFile -> replaceTemplatePlaceholdersInFileOnDisk appName projectName absMainWaspFile

-- | Template file for package.json file has placeholders in it that we want to replace
-- in the package.json file we have written to the disk.
-- If no package.json file was found in the project, do nothing.
replaceTemplatePlaceholdersInPackageJsonFile ::
  NewProjectAppName -> NewProjectName -> Path' Abs (Dir WaspProjectDir) -> IO ()
replaceTemplatePlaceholdersInPackageJsonFile appName projectName projectDir =
  findPackageJsonFile projectDir >>= \case
    Nothing -> return ()
    Just absPackageJsonFile -> replaceTemplatePlaceholdersInFileOnDisk appName projectName absPackageJsonFile

replaceTemplatePlaceholdersInFileOnDisk :: NewProjectAppName -> NewProjectName -> Path' Abs (File f) -> IO ()
replaceTemplatePlaceholdersInFileOnDisk appName projectName =
  updateFileContentWith (replacePlaceholders waspTemplateReplacements)
  where
    updateFileContentWith :: (Text -> Text) -> Path' Abs (File f) -> IO ()
    updateFileContentWith updateFn absFilePath = IOUtil.readFileStrict absFilePath >>= IOUtil.writeFileFromText absFilePath . updateFn

    replacePlaceholders :: [(String, String)] -> Text -> Text
    replacePlaceholders replacements content = foldl' replacePlaceholder content replacements
      where
        replacePlaceholder content' (placeholder, value) = T.replace (T.pack placeholder) (T.pack value) content'

    waspTemplateReplacements =
      [ ("__waspAppName__", show appName),
        ("__waspProjectName__", show projectName),
        ("__waspVersion__", defaultWaspVersionBounds)
      ]
