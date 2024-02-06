module Wasp.Cli.Command.CreateNewProject.StarterTemplates.Local
  ( createProjectOnDiskFromLocalTemplate,
  )
where

import Path.IO (copyDirRecur)
import StrongPath (Abs, Dir, Dir', Path', Rel', reldir, (</>))
import StrongPath.Path (toPathAbsDir)
import Wasp.Cli.Command.CreateNewProject.ProjectDescription (NewProjectAppName, NewProjectName)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Templating (replaceTemplatePlaceholdersInTemplateFiles)
import qualified Wasp.Data as Data
import Wasp.Project (WaspProjectDir)

createProjectOnDiskFromLocalTemplate ::
  Path' Abs (Dir WaspProjectDir) -> NewProjectName -> NewProjectAppName -> Path' Rel' Dir' -> IO ()
createProjectOnDiskFromLocalTemplate absWaspProjectDir projectName appName templatePath = do
  copyLocalTemplateToNewProjectDir templatePath
  replaceTemplatePlaceholdersInTemplateFiles appName projectName absWaspProjectDir
  where
    copyLocalTemplateToNewProjectDir :: Path' Rel' Dir' -> IO ()
    copyLocalTemplateToNewProjectDir templateDir = do
      dataDir <- Data.getAbsDataDirPath
      let absLocalTemplateDir =
            dataDir </> [reldir|Cli/templates|] </> templateDir
      let absSkeletonTemplateDir =
            dataDir </> [reldir|Cli/templates/skeleton|]
      -- First we copy skeleton files, which form the basis of any Wasp project,
      -- and then on top of that we add files specific to the specified local template.
      copyDirRecur (toPathAbsDir absSkeletonTemplateDir) (toPathAbsDir absWaspProjectDir)
      copyDirRecur (toPathAbsDir absLocalTemplateDir) (toPathAbsDir absWaspProjectDir)
