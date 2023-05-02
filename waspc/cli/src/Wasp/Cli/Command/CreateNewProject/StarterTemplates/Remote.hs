module Wasp.Cli.Command.CreateNewProject.StarterTemplates.Remote
  ( createProjectOnDiskFromRemoteTemplate,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.CreateNewProject.Common (throwProjectCreationError)
import Wasp.Cli.Command.CreateNewProject.ProjectDescription (NewProjectAppName, NewProjectName)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Remote.Github (starterTemplateGithubRepo)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Templating (replaceTemplatePlaceholdersInWaspFile)
import Wasp.Cli.GithubRepo (fetchFolderFromGithubRepoToDisk)
import Wasp.Project (WaspProjectDir)

createProjectOnDiskFromRemoteTemplate ::
  Path' Abs (Dir WaspProjectDir) ->
  NewProjectName ->
  NewProjectAppName ->
  String ->
  Command ()
createProjectOnDiskFromRemoteTemplate absWaspProjectDir projectName appName templateName = do
  fetchGithubTemplateToDisk absWaspProjectDir templateName >>= either throwProjectCreationError pure
  liftIO $ replaceTemplatePlaceholdersInWaspFile appName projectName absWaspProjectDir
  where
    fetchGithubTemplateToDisk :: Path' Abs (Dir WaspProjectDir) -> String -> Command (Either String ())
    fetchGithubTemplateToDisk projectDir templateFolderName = do
      let templateFolderPath = fromJust . SP.parseRelDir $ templateFolderName
      liftIO $ fetchFolderFromGithubRepoToDisk starterTemplateGithubRepo templateFolderPath projectDir
