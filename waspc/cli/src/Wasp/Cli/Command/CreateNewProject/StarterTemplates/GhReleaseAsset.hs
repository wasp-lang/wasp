module Wasp.Cli.Command.CreateNewProject.StarterTemplates.GhReleaseAsset
  ( createProjectOnDiskFromGhReleaseAssetTemplate,
  )
where

import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Dir', Path', Rel')
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.CreateNewProject.Common (throwProjectCreationError)
import Wasp.Cli.Command.CreateNewProject.ProjectDescription (NewProjectAppName, NewProjectName)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Templating (replaceTemplatePlaceholdersInTemplateFiles)
import Wasp.Cli.GithubRepo (GithubReleaseAssetName, GithubRepoRef, fetchFolderFromGithubReleaseAssetToDisk)
import Wasp.Project (WaspProjectDir)

createProjectOnDiskFromGhReleaseAssetTemplate ::
  Path' Abs (Dir WaspProjectDir) ->
  NewProjectName ->
  NewProjectAppName ->
  GithubRepoRef ->
  GithubReleaseAssetName ->
  Path' Rel' Dir' ->
  Command ()
createProjectOnDiskFromGhReleaseAssetTemplate absWaspProjectDir projectName appName ghRepoRef assetName templatePathInRepo = do
  fetchTheTemplateFromGhToDisk >>= either throwProjectCreationError pure
  liftIO $ replaceTemplatePlaceholdersInTemplateFiles appName projectName absWaspProjectDir
  where
    fetchTheTemplateFromGhToDisk = do
      liftIO $ fetchFolderFromGithubReleaseAssetToDisk ghRepoRef assetName templatePathInRepo absWaspProjectDir
