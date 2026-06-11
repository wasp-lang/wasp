module Wasp.Cli.Command.CreateNewProject.StarterTemplates.GhReleaseArchive
  ( createProjectOnDiskFromGhReleaseArchiveTemplate,
  )
where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import StrongPath (Dir', Path', Rel')
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.CreateNewProject.Common (throwProjectCreationError)
import Wasp.Cli.Command.CreateNewProject.ProjectDescription (NewProjectDescription (..), getAbsWaspProjectDir)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Templating (replaceTemplatePlaceholdersInTemplateFiles)
import Wasp.Cli.GithubRepo
  ( GithubReleaseArchiveName,
    GithubRepoRef (_repoName, _repoOwner),
    checkGitHubReleaseExists,
    fetchFolderFromGithubReleaseArchiveToDisk,
  )
import Wasp.Util (indent)
import Wasp.Util.InstallMethod (getInstallationCommand)

createProjectOnDiskFromGhReleaseArchiveTemplate ::
  NewProjectDescription ->
  GithubRepoRef ->
  GithubReleaseArchiveName ->
  Path' Rel' Dir' ->
  Command ()
createProjectOnDiskFromGhReleaseArchiveTemplate newProjectDescription ghRepoRef assetName templatePathInRepo = do
  releaseExists <- liftIO (checkGitHubReleaseExists ghRepoRef)

  unless releaseExists throwOutdatedTagError

  fetchTemplateFromGhToTemplateOutputDir
    >>= either throwProjectCreationError (const replaceTemplatePlaceholders)
  where
    fetchTemplateFromGhToTemplateOutputDir =
      liftIO $ fetchFolderFromGithubReleaseArchiveToDisk ghRepoRef assetName templatePathInRepo (_absTemplateOutputDir newProjectDescription)

    replaceTemplatePlaceholders =
      liftIO $ replaceTemplatePlaceholdersInTemplateFiles (_appName newProjectDescription) (_projectName newProjectDescription) absWaspProjectDir

    throwOutdatedTagError =
      throwProjectCreationError $
        unlines
          [ "Template " ++ show templateName ++ " doesn't yet have a version compatible with the current Wasp version.",
            "You can try again with an older Wasp version, or choose a different template.",
            "",
            "If you want to use an older Wasp version:",
            "Visit " ++ releasesUrl ++ " to see available template releases,",
            "and install the Wasp version that matches the latest release tag available by running:",
            "",
            indent 2 $ getInstallationCommand (Just "x.y.z"),
            "",
            "Then you can try creating your project again."
          ]

    releasesUrl = intercalate "/" ["https://github.com", _repoOwner ghRepoRef, _repoName ghRepoRef, "releases"]

    templateName = show $ _template newProjectDescription
    absWaspProjectDir = getAbsWaspProjectDir newProjectDescription
