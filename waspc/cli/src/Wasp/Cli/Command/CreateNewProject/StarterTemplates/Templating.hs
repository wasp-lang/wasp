module Wasp.Cli.Command.CreateNewProject.StarterTemplates.Templating
  ( replaceTemplatePlaceholdersInTemplateFiles,
  )
where

import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import StrongPath (Abs, Dir, File, Path')
import Wasp.Cli.Command.CreateNewProject.Common (defaultWaspVersionBounds)
import Wasp.Cli.Command.CreateNewProject.ProjectDescription (NewProjectAppName (..), NewProjectName)
import Wasp.NodePackageFFI (InstallablePackage (WaspConfigPackage), getPackageInstallationPath)
import Wasp.Project.Analyze (WaspFilePath (..))
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Project.ExternalConfig.PackageJson (findPackageJsonFile)
import Wasp.Project.WaspFile (findWaspFile)
import Wasp.Util (camelToKebabCase)
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
    Left _error -> return ()
    Right (WaspLang absMainWaspFile) -> replaceTemplatePlaceholders absMainWaspFile
    Right (WaspTs absMainTsFile) -> replaceTemplatePlaceholders absMainTsFile
  where
    replaceTemplatePlaceholders = replaceTemplatePlaceholdersInFileOnDisk appName projectName

-- | Template file for package.json file has placeholders in it that we want to replace
-- in the package.json file we have written to the disk.
-- If no package.json file was found in the project, do nothing.
replaceTemplatePlaceholdersInPackageJsonFile ::
  NewProjectAppName -> NewProjectName -> Path' Abs (Dir WaspProjectDir) -> IO ()
replaceTemplatePlaceholdersInPackageJsonFile appName projectName projectDir =
  findPackageJsonFile projectDir >>= \case
    Nothing -> return ()
    Just absPackageJsonFile -> do
      let kebabCaseAppName = NewProjectAppName . camelToKebabCase . show $ appName
      replaceTemplatePlaceholdersInFileOnDisk kebabCaseAppName projectName absPackageJsonFile

replaceTemplatePlaceholdersInFileOnDisk :: NewProjectAppName -> NewProjectName -> Path' Abs (File f) -> IO ()
replaceTemplatePlaceholdersInFileOnDisk appName projectName file = do
  waspConfigPackagePath <- getPackageInstallationPath WaspConfigPackage
  let waspTemplateReplacements =
        [ ("__waspConfigPath__", waspConfigPackagePath),
          ("__waspAppName__", show appName),
          ("__waspProjectName__", show projectName),
          ("__waspVersion__", defaultWaspVersionBounds)
        ]
  updateFileContentWith (replacePlaceholders waspTemplateReplacements) file
  where
    updateFileContentWith :: (Text -> Text) -> Path' Abs (File f) -> IO ()
    updateFileContentWith updateFn absFilePath = IOUtil.readFileStrict absFilePath >>= IOUtil.writeFileFromText absFilePath . updateFn

    replacePlaceholders :: [(String, String)] -> Text -> Text
    replacePlaceholders replacements content = foldl' replacePlaceholder content replacements
      where
        replacePlaceholder content' (placeholder, value) = T.replace (T.pack placeholder) (T.pack value) content'
