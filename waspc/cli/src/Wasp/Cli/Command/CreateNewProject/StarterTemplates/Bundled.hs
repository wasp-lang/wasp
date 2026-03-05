module Wasp.Cli.Command.CreateNewProject.StarterTemplates.Bundled
  ( createProjectOnDiskFromBundledTemplate,
  )
where

import Path.IO (copyDirRecur)
import StrongPath (Abs, Dir, Dir', Path', Rel', reldir, (</>))
import StrongPath.Path (toPathAbsDir)
import Wasp.Cli.Command.CreateNewProject.ProjectDescription (NewProjectAppName, NewProjectName)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Templating (replaceTemplatePlaceholdersInTemplateFiles)
import qualified Wasp.Data as Data
import Wasp.Project (WaspProjectDir)

createProjectOnDiskFromBundledTemplate ::
  Path' Abs (Dir WaspProjectDir) -> NewProjectName -> NewProjectAppName -> Path' Rel' Dir' -> IO ()
createProjectOnDiskFromBundledTemplate absWaspProjectDir projectName appName templatePath = do
  copyBundledTemplateToNewProjectDir templatePath
  replaceTemplatePlaceholdersInTemplateFiles appName projectName absWaspProjectDir
  where
    copyBundledTemplateToNewProjectDir :: Path' Rel' Dir' -> IO ()
    copyBundledTemplateToNewProjectDir templateDir = do
      dataDir <- Data.getAbsDataDirPath
      let absLocalTemplateDir =
            dataDir </> [reldir|Cli/starters|] </> templateDir
      let absSkeletonTemplateDir =
            dataDir </> [reldir|Cli/starters/skeleton|]
      -- First we copy skeleton files, which form the basis of any Wasp project,
      -- and then on top of that we add files specific to the specified local template.
      copyDirRecur (toPathAbsDir absSkeletonTemplateDir) (toPathAbsDir absWaspProjectDir)
      copyDirRecur (toPathAbsDir absLocalTemplateDir) (toPathAbsDir absWaspProjectDir)
