module Wasp.Cli.Command.CreateNewProject
  ( createNewProject,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import qualified StrongPath as SP
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Call (Arguments)
import qualified Wasp.Cli.Command.CreateNewProject.AI as AI
import Wasp.Cli.Command.CreateNewProject.ArgumentsParser (newProjectArgsParser)
import qualified Wasp.Cli.Command.CreateNewProject.Common as Common
import Wasp.Cli.Command.CreateNewProject.ProjectDescription
  ( NewProjectDescription (..),
    obtainNewProjectDescription,
  )
import Wasp.Cli.Command.CreateNewProject.StarterTemplates
  ( DirBasedTemplateMetadata (_path),
    StarterTemplate (..),
    availableStarterTemplates,
    getTemplateStartingInstructions,
  )
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.GhRepo (createProjectOnDiskFromGhRepoTemplate)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Local (createProjectOnDiskFromLocalTemplate)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Util.Parser (parseArguments)
import qualified Wasp.Message as Msg
import qualified Wasp.Util.Terminal as Term

-- | It receives all of the arguments that were passed to the `wasp new` command.
createNewProject :: Arguments -> Command ()
createNewProject args = do
  newProjectArgs <-
    parseArguments "wasp new" newProjectArgsParser args
      & either Common.throwProjectCreationError return

  newProjectDescription <- obtainNewProjectDescription newProjectArgs availableStarterTemplates

  createProjectOnDisk newProjectDescription
  liftIO $ printGettingStartedInstructionsForProject newProjectDescription

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

-- | This function assumes that the project dir was created inside the current working directory.
printGettingStartedInstructionsForProject :: NewProjectDescription -> IO ()
printGettingStartedInstructionsForProject projectDescription = do
  let projectDirName = init . SP.toFilePath . SP.basename $ _absWaspProjectDir projectDescription
  let instructions = getTemplateStartingInstructions projectDirName $ _template projectDescription
  putStrLn $ Term.applyStyles [Term.Green] $ "Created new Wasp app in ./" ++ projectDirName ++ " directory!"
  putStrLn ""
  putStrLn instructions
