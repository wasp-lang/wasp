module Wasp.Cli.Command.CreateNewProject
  ( createNewProject,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Call (Arguments)
import qualified Wasp.Cli.Command.CreateNewProject.AI as AI
import Wasp.Cli.Command.CreateNewProject.ArgumentsParser (parseNewProjectArgs)
import Wasp.Cli.Command.CreateNewProject.Common (printGettingStartedInstructions, throwProjectCreationError)
import Wasp.Cli.Command.CreateNewProject.ProjectDescription
  ( NewProjectDescription (..),
    obtainNewProjectDescription,
  )
import Wasp.Cli.Command.CreateNewProject.StarterTemplates
  ( DirBasedTemplateMetadata (_path),
    StarterTemplate (..),
    getStarterTemplates,
  )
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.GhRepo (createProjectOnDiskFromGhRepoTemplate)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Local (createProjectOnDiskFromLocalTemplate)
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Message as Msg

-- | It receives all of the arguments that were passed to the `wasp new` command.
createNewProject :: Arguments -> Command ()
createNewProject args = do
  newProjectArgs <- parseNewProjectArgs args & either throwProjectCreationError return
  starterTemplates <- liftIO getStarterTemplates

  newProjectDescription <- obtainNewProjectDescription newProjectArgs starterTemplates

  createProjectOnDisk newProjectDescription
  liftIO $ printGettingStartedInstructions $ _absWaspProjectDir newProjectDescription

createProjectOnDisk :: NewProjectDescription -> Command ()
createProjectOnDisk
  NewProjectDescription
    { _projectName = projectName,
      _appName = appName,
      _template = template,
      _absWaspProjectDir = absWaspProjectDir
    } = do
    cliSendMessageC $ Msg.Start $ "Creating your project from the \"" ++ show template ++ "\" template..."
    case template of
      GhRepoStarterTemplate ghRepoRef metadata ->
        createProjectOnDiskFromGhRepoTemplate absWaspProjectDir projectName appName ghRepoRef $ _path metadata
      LocalStarterTemplate metadata ->
        liftIO $ createProjectOnDiskFromLocalTemplate absWaspProjectDir projectName appName $ _path metadata
      AiGeneratedStarterTemplate ->
        AI.createNewProjectInteractiveOnDisk absWaspProjectDir appName
