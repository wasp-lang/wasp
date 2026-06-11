module Wasp.Cli.Command.CreateNewProject.StarterTemplates.Bundled
  ( createProjectOnDiskFromBundledTemplate,
  )
where

import Path.IO (copyDirRecur)
import StrongPath (Abs, Dir, Dir', Path', Rel', fromAbsDir, reldir, (</>))
import StrongPath.Path (toPathAbsDir)
import System.Directory (renameFile)
import qualified System.FilePath as FP
import Wasp.Cli.Command.CreateNewProject.ProjectDescription (NewProjectDescription (..), getAbsWaspProjectDir)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates (skeletonDotfiles)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Templating (replaceTemplatePlaceholdersInTemplateFiles)
import qualified Wasp.Data as Data
import Wasp.Project (WaspProjectDir)

createProjectOnDiskFromBundledTemplate ::
  NewProjectDescription -> Path' Rel' Dir' -> IO ()
createProjectOnDiskFromBundledTemplate newProjectDescription templatePath = do
  copyBundledTemplateToNewProjectDir templatePath
  replaceTemplatePlaceholdersInTemplateFiles (_appName newProjectDescription) (_projectName newProjectDescription) absWaspProjectDir
  where
    absWaspProjectDir = getAbsWaspProjectDir newProjectDescription

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
      renameDotfiles absWaspProjectDir skeletonDotfiles
      copyDirRecur (toPathAbsDir absLocalTemplateDir) (toPathAbsDir absWaspProjectDir)

    renameDotfiles :: Path' Abs (Dir WaspProjectDir) -> [String] -> IO ()
    renameDotfiles projectDir dotfiles = do
      let dir = fromAbsDir projectDir
      mapM_ (\name -> renameFile (dir FP.</> name) (dir FP.</> ("." <> name))) dotfiles
