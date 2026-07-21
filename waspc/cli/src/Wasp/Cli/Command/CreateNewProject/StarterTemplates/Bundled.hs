module Wasp.Cli.Command.CreateNewProject.StarterTemplates.Bundled
  ( createProjectOnDiskFromBundledTemplate,
  )
where

import Path.IO (copyDirRecur)
import StrongPath (Dir', Path', Rel', reldir, (</>))
import StrongPath.Path (toPathAbsDir)
import Wasp.Cli.Command.CreateNewProject.ProjectDescription (NewProjectDescription (..), getAbsWaspProjectDir)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates (skeletonDotfiles)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Templating (replaceTemplatePlaceholdersInTemplateFiles)
import qualified Wasp.Data as Data
import qualified Wasp.Util.IO as IOUtil

createProjectOnDiskFromBundledTemplate ::
  NewProjectDescription -> Path' Rel' Dir' -> IO ()
createProjectOnDiskFromBundledTemplate newProjectDescription templatePath = do
  copyBundledTemplateToNewProjectDir templatePath
  replaceTemplatePlaceholdersInTemplateFiles (_appName newProjectDescription) (_projectName newProjectDescription) absWaspProjectDir
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
      IOUtil.renameDotfiles absWaspProjectDir skeletonDotfiles
      copyDirRecur (toPathAbsDir absLocalTemplateDir) (toPathAbsDir absWaspProjectDir)

    absWaspProjectDir = getAbsWaspProjectDir newProjectDescription
