module Wasp.Cli.Command.CreateNewProject.StarterTemplates.Templating
  ( replaceTemplatePlaceholdersInTemplateFiles,
  )
where

import Control.Lens (at, (&), (?~))
import Control.Monad (forM_)
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson.Key as Key
import Data.Aeson.Lens (_Object)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import StrongPath (Abs, Dir, File, Path')
import Wasp.Cli.Command.CreateNewProject.ProjectDescription (NewProjectAppName (..), NewProjectName)
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Project.ExternalConfig.PackageJson (findUserPackageJsonFile)
import Wasp.Project.WaspFile (findWaspFile)
import Wasp.Project.WaspNpmPackage
  ( DevPackages,
    getDevPackageOverrides,
    getDevPackages,
    getWaspNpmPackageSpecifier,
  )
import Wasp.Util (camelToKebabCase)
import qualified Wasp.Util.IO as IOUtil
import Wasp.Util.Json (updateJsonFile)

replaceTemplatePlaceholdersInTemplateFiles :: NewProjectAppName -> NewProjectName -> Path' Abs (Dir WaspProjectDir) -> IO ()
replaceTemplatePlaceholdersInTemplateFiles appName projectName projectDir = do
  devPackages <- getDevPackages
  replaceTemplatePlaceholdersInWaspFile devPackages appName projectName projectDir
  replaceTemplatePlaceholdersInPackageJsonFile devPackages appName projectName projectDir

-- | Template file for wasp file has placeholders in it that we want to replace
-- in the .wasp file we have written to the disk.
-- If no .wasp file was found in the project, do nothing.
replaceTemplatePlaceholdersInWaspFile ::
  Maybe DevPackages -> NewProjectAppName -> NewProjectName -> Path' Abs (Dir WaspProjectDir) -> IO ()
replaceTemplatePlaceholdersInWaspFile devPackages appName projectName projectDir =
  findWaspFile projectDir >>= \case
    Left _error -> return ()
    Right absMainTsFile -> replaceTemplatePlaceholders absMainTsFile
  where
    replaceTemplatePlaceholders = replaceTemplatePlaceholdersInFileOnDisk devPackages appName projectName

-- | Template file for package.json file has placeholders in it that we want to replace
-- in the package.json file we have written to the disk.
-- If no package.json file was found in the project, do nothing.
replaceTemplatePlaceholdersInPackageJsonFile ::
  Maybe DevPackages -> NewProjectAppName -> NewProjectName -> Path' Abs (Dir WaspProjectDir) -> IO ()
replaceTemplatePlaceholdersInPackageJsonFile devPackages appName projectName projectDir =
  findUserPackageJsonFile projectDir >>= \case
    Nothing -> return ()
    Just absPackageJsonFile -> do
      let kebabCaseAppName = NewProjectAppName . camelToKebabCase . show $ appName
      replaceTemplatePlaceholdersInFileOnDisk devPackages kebabCaseAppName projectName absPackageJsonFile
      forM_ devPackages $ \devPackages' ->
        updateJsonFile (addDevPackageOverrides devPackages') absPackageJsonFile
          >>= either (ioError . userError) return

-- | The dev @wasp/wasp tarball pins unpublished versions of @wasp/cli and
-- @wasp/spec, so the project needs npm `overrides` pointing at their tarballs.
-- TODO: Remove once the repo's Wasp apps join the pnpm workspace.
addDevPackageOverrides :: DevPackages -> Value -> Value
addDevPackageOverrides devPackages packageJson =
  packageJson & _Object . at (Key.fromString "overrides") ?~ overrides
  where
    overrides = object [Key.fromString name .= T.pack specifier | (name, specifier) <- getDevPackageOverrides devPackages]

replaceTemplatePlaceholdersInFileOnDisk :: Maybe DevPackages -> NewProjectAppName -> NewProjectName -> Path' Abs (File f) -> IO ()
replaceTemplatePlaceholdersInFileOnDisk devPackages appName projectName file = do
  let waspTemplateReplacements =
        [ ("__waspPackageSpecifier__", getWaspNpmPackageSpecifier devPackages),
          ("__waspAppName__", show appName),
          ("__waspProjectName__", show projectName)
        ]
  updateFileContentWith (replacePlaceholders waspTemplateReplacements) file
  where
    updateFileContentWith :: (Text -> Text) -> Path' Abs (File f) -> IO ()
    updateFileContentWith updateFn absFilePath = IOUtil.readFileStrict absFilePath >>= IOUtil.writeFileFromText absFilePath . updateFn

    replacePlaceholders :: [(String, String)] -> Text -> Text
    replacePlaceholders replacements content = foldl' replacePlaceholder content replacements
      where
        replacePlaceholder content' (placeholder, value) = T.replace (T.pack placeholder) (T.pack value) content'
