module Wasp.Cli.Command.CreateNewProject.StarterTemplates.GhStartersRepo
  ( createProjectOnDiskFromGhStartersRepoTemplate,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.CreateNewProject.Common (throwProjectCreationError)
import Wasp.Cli.Command.CreateNewProject.ProjectDescription (NewProjectAppName, NewProjectName)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.GhStartersRepo.Github (startersRepo)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Templating (replaceTemplatePlaceholdersInWaspFile)
import Wasp.Cli.GithubRepo (fetchFolderFromGithubRepoToDisk)
import Wasp.Project (WaspProjectDir)

createProjectOnDiskFromGhStartersRepoTemplate ::
  Path' Abs (Dir WaspProjectDir) ->
  NewProjectName ->
  NewProjectAppName ->
  String ->
  Command ()
createProjectOnDiskFromGhStartersRepoTemplate absWaspProjectDir projectName appName templatePath = do
  fetchGithubTemplateToDisk absWaspProjectDir templatePath >>= either throwProjectCreationError pure
  liftIO $ replaceTemplatePlaceholdersInWaspFile appName projectName absWaspProjectDir
  where
    fetchGithubTemplateToDisk :: Path' Abs (Dir WaspProjectDir) -> String -> Command (Either String ())
    fetchGithubTemplateToDisk projectDir templatePathInRepo = do
      let templateFolderPath = fromJust . SP.parseRelDir $ templatePathInRepo
      liftIO $ fetchFolderFromGithubRepoToDisk startersRepo templateFolderPath projectDir
