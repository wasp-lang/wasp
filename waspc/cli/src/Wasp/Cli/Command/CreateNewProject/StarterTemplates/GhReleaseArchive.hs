module Wasp.Cli.Command.CreateNewProject.StarterTemplates.GhReleaseArchive
  ( createProjectOnDiskFromGhReleaseArchiveTemplate,
  )
where

import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Dir', Path', Rel')
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.CreateNewProject.Common (throwProjectCreationError)
import Wasp.Cli.Command.CreateNewProject.ProjectDescription (NewProjectAppName, NewProjectName)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Templating (replaceTemplatePlaceholdersInTemplateFiles)
import Wasp.Cli.GithubRepo (GithubReleaseArchiveName, GithubRepoRef, fetchFolderFromGithubReleaseArchiveToDisk)
import Wasp.Project (WaspProjectDir)

createProjectOnDiskFromGhReleaseArchiveTemplate ::
  Path' Abs (Dir WaspProjectDir) ->
  NewProjectName ->
  NewProjectAppName ->
  GithubRepoRef ->
  GithubReleaseArchiveName ->
  Path' Rel' Dir' ->
  Command ()
createProjectOnDiskFromGhReleaseArchiveTemplate absWaspProjectDir projectName appName ghRepoRef assetName templatePathInRepo = do
  fetchTheTemplateFromGhToDisk >>= either throwProjectCreationError pure
  liftIO $ replaceTemplatePlaceholdersInTemplateFiles appName projectName absWaspProjectDir
  where
    fetchTheTemplateFromGhToDisk = do
      liftIO $ fetchFolderFromGithubReleaseArchiveToDisk ghRepoRef assetName templatePathInRepo absWaspProjectDir
