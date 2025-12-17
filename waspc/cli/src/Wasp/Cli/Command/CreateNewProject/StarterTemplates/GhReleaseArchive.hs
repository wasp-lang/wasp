module Wasp.Cli.Command.CreateNewProject.StarterTemplates.GhReleaseArchive
  ( createProjectOnDiskFromGhReleaseArchiveTemplate,
  )
where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import StrongPath (Abs, Dir, Dir', Path', Rel')
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.CreateNewProject.Common (throwProjectCreationError)
import Wasp.Cli.Command.CreateNewProject.ProjectDescription (NewProjectAppName, NewProjectName)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Templating (replaceTemplatePlaceholdersInTemplateFiles)
import Wasp.Cli.GithubRepo
  ( GithubReleaseArchiveName,
    GithubRepoRef (_repoName, _repoOwner),
    checkGitHubReleaseExists,
    fetchFolderFromGithubReleaseArchiveToDisk,
  )
import Wasp.Project (WaspProjectDir)

createProjectOnDiskFromGhReleaseArchiveTemplate ::
  String ->
  Path' Abs (Dir WaspProjectDir) ->
  NewProjectName ->
  NewProjectAppName ->
  GithubRepoRef ->
  GithubReleaseArchiveName ->
  Path' Rel' Dir' ->
  Command ()
createProjectOnDiskFromGhReleaseArchiveTemplate templateName absWaspProjectDir projectName appName ghRepoRef assetName templatePathInRepo = do
  releaseExists <- liftIO (checkGitHubReleaseExists ghRepoRef)

  unless releaseExists throwOutdatedTagError

  fetchTemplateFromGhToWaspProjectDir
    >>= either throwProjectCreationError (const replaceTemplatePlaceholders)
  where
    fetchTemplateFromGhToWaspProjectDir =
      liftIO $ fetchFolderFromGithubReleaseArchiveToDisk ghRepoRef assetName templatePathInRepo absWaspProjectDir

    replaceTemplatePlaceholders =
      liftIO $ replaceTemplatePlaceholdersInTemplateFiles appName projectName absWaspProjectDir

    throwOutdatedTagError =
      throwProjectCreationError $
        unlines
          [ "Template " ++ templateName ++ " doesn't yet have a version compatible with the current Wasp version.",
            "You can try again with an older Wasp version, or choose a different template.",
            "",
            "If you want to use an older Wasp version:",
            "Visit " ++ releasesUrl ++ " to see available releases,",
            "and install the Wasp version that matches the latest release tag available by running:",
            "  curl -sSL https://get.wasp.sh/installer.sh | sh -s -- -v x.y.z",
            "Then you can try creating your project again."
          ]

    releasesUrl = intercalate "/" ["https://github.com", _repoOwner ghRepoRef, _repoName ghRepoRef, "releases"]
