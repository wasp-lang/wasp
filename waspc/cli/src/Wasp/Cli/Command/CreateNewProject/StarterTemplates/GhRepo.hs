module Wasp.Cli.Command.CreateNewProject.StarterTemplates.GhRepo
  ( createProjectOnDiskFromGhRepoTemplate,
  )
where

import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Dir', Path', Rel')
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.CreateNewProject.Common (throwProjectCreationError)
import Wasp.Cli.Command.CreateNewProject.ProjectDescription (NewProjectAppName, NewProjectName)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Templating (replaceTemplatePlaceholdersInTemplateFiles)
import Wasp.Cli.GithubRepo (GithubRepoRef, fetchFolderFromGithubRepoToDisk)
import Wasp.Project (WaspProjectDir)

createProjectOnDiskFromGhRepoTemplate ::
  Path' Abs (Dir WaspProjectDir) ->
  NewProjectName ->
  NewProjectAppName ->
  GithubRepoRef ->
  Path' Rel' Dir' ->
  Command ()
createProjectOnDiskFromGhRepoTemplate absWaspProjectDir projectName appName ghRepoRef templatePathInRepo = do
  fetchTheTemplateFromGhToDisk >>= either throwProjectCreationError pure
  liftIO $ replaceTemplatePlaceholdersInTemplateFiles appName projectName absWaspProjectDir
  where
    fetchTheTemplateFromGhToDisk = do
      liftIO $ fetchFolderFromGithubRepoToDisk ghRepoRef templatePathInRepo absWaspProjectDir
