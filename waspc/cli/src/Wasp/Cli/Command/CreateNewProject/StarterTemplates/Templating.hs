module Wasp.Cli.Command.CreateNewProject.StarterTemplates.Templating
  ( replaceTemplatePlaceholdersInTemplateFiles,
  )
where

import Control.Monad (forM_)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import StrongPath (Abs, Dir, File, Path')
import Wasp.Cli.Command.CreateNewProject.ProjectDescription (NewProjectAppName (..), NewProjectName)
import Wasp.Cli.DevWaspNpmPackages
  ( DevWaspNpmPackageTarballs,
    addDevWaspNpmPackageOverridesToPackageJsonFile,
    devWaspNpmPackageSpecifier,
    findDevWaspNpmPackageTarballs,
  )
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Project.ExternalConfig.PackageJson (findUserPackageJsonFile)
import Wasp.Project.WaspFile (findWaspFile)
import Wasp.Project.WaspNpmPackage (getWaspCliNpmPackageVersion)
import Wasp.Util (camelToKebabCase)
import qualified Wasp.Util.IO as IOUtil

replaceTemplatePlaceholdersInTemplateFiles :: NewProjectAppName -> NewProjectName -> Path' Abs (Dir WaspProjectDir) -> IO ()
replaceTemplatePlaceholdersInTemplateFiles appName projectName projectDir = do
  devTarballs <- findDevWaspNpmPackageTarballs
  waspNpmPackageVersion <- maybe (show <$> getWaspCliNpmPackageVersion) (return . devWaspNpmPackageSpecifier) devTarballs
  replaceTemplatePlaceholdersInWaspFile appName projectName projectDir
  replaceTemplatePlaceholdersInPackageJsonFile waspNpmPackageVersion appName projectName projectDir
  forM_ devTarballs $ addDevWaspNpmPackageOverridesToProject projectDir

-- | Template file for wasp file has placeholders in it that we want to replace
-- in the .wasp file we have written to the disk.
-- If no .wasp file was found in the project, do nothing.
replaceTemplatePlaceholdersInWaspFile ::
  NewProjectAppName -> NewProjectName -> Path' Abs (Dir WaspProjectDir) -> IO ()
replaceTemplatePlaceholdersInWaspFile appName projectName projectDir =
  findWaspFile projectDir >>= \case
    Left _error -> return ()
    Right absMainTsFile ->
      replaceTemplatePlaceholdersInFileOnDisk
        [ ("__waspAppName__", show appName),
          ("__waspProjectName__", show projectName)
        ]
        absMainTsFile

-- | Template file for package.json file has placeholders in it that we want to replace
-- in the package.json file we have written to the disk.
-- If no package.json file was found in the project, do nothing.
replaceTemplatePlaceholdersInPackageJsonFile ::
  String -> NewProjectAppName -> NewProjectName -> Path' Abs (Dir WaspProjectDir) -> IO ()
replaceTemplatePlaceholdersInPackageJsonFile waspNpmPackageVersion appName projectName projectDir =
  findUserPackageJsonFile projectDir >>= \case
    Nothing -> return ()
    Just absPackageJsonFile ->
      replaceTemplatePlaceholdersInFileOnDisk
        [ ("__waspNpmPackageVersion__", waspNpmPackageVersion),
          ("__waspAppName__", camelToKebabCase $ show appName),
          ("__waspProjectName__", show projectName)
        ]
        absPackageJsonFile

-- | If no package.json file was found in the project, do nothing.
addDevWaspNpmPackageOverridesToProject :: Path' Abs (Dir WaspProjectDir) -> DevWaspNpmPackageTarballs -> IO ()
addDevWaspNpmPackageOverridesToProject projectDir devTarballs =
  findUserPackageJsonFile projectDir
    >>= mapM_ (addDevWaspNpmPackageOverridesToPackageJsonFile devTarballs)

-- | Replaces each placeholder in the file with its value.
replaceTemplatePlaceholdersInFileOnDisk :: [(String, String)] -> Path' Abs (File f) -> IO ()
replaceTemplatePlaceholdersInFileOnDisk placeholdersWithValues file =
  updateFileContentWith (replacePlaceholders placeholdersWithValues) file
  where
    updateFileContentWith :: (Text -> Text) -> Path' Abs (File f) -> IO ()
    updateFileContentWith updateFn absFilePath = IOUtil.readFileStrict absFilePath >>= IOUtil.writeFileFromText absFilePath . updateFn

    replacePlaceholders :: [(String, String)] -> Text -> Text
    replacePlaceholders replacements content = foldl' replacePlaceholder content replacements
      where
        replacePlaceholder content' (placeholder, value) = T.replace (T.pack placeholder) (T.pack value) content'
