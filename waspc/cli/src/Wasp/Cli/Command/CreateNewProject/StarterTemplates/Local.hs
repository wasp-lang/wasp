module Wasp.Cli.Command.CreateNewProject.StarterTemplates.Local
  ( createProjectOnDiskFromLocalTemplate,
  )
where

import Data.Maybe (fromJust)
import Path.IO (copyDirRecur)
import StrongPath (Abs, Dir, Path', reldir, (</>))
import qualified StrongPath as SP
import StrongPath.Path (toPathAbsDir)
import Wasp.Cli.Command.CreateNewProject.ProjectDescription (NewProjectAppName, NewProjectName)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Templating (replaceTemplatePlaceholdersInWaspFile)
import qualified Wasp.Data as Data
import Wasp.Project (WaspProjectDir)

createProjectOnDiskFromLocalTemplate ::
  Path' Abs (Dir WaspProjectDir) -> NewProjectName -> NewProjectAppName -> String -> IO ()
createProjectOnDiskFromLocalTemplate absWaspProjectDir projectName appName templateName = do
  copyLocalTemplateToNewProjectDir templateName
  replaceTemplatePlaceholdersInWaspFile appName projectName absWaspProjectDir
  where
    copyLocalTemplateToNewProjectDir :: String -> IO ()
    copyLocalTemplateToNewProjectDir templateDir = do
      dataDir <- Data.getAbsDataDirPath
      let absLocalTemplateDir =
            dataDir </> [reldir|Cli/templates|] </> (fromJust . SP.parseRelDir $ templateDir)
      let absSkeletonTemplateDir =
            dataDir </> [reldir|Cli/templates/skeleton|]
      -- First we copy skeleton files, which form the basis of any Wasp project,
      -- and then on top of that we add files specific to the specified local template.
      copyDirRecur (toPathAbsDir absSkeletonTemplateDir) (toPathAbsDir absWaspProjectDir)
      copyDirRecur (toPathAbsDir absLocalTemplateDir) (toPathAbsDir absWaspProjectDir)
