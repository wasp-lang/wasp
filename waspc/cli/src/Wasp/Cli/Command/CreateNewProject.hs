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
import Wasp.Cli.Command.CreateNewProject.ArgumentsParser (parseNewProjectArgs)
import qualified Wasp.Cli.Command.CreateNewProject.Common as Common
import Wasp.Cli.Command.CreateNewProject.ProjectDescription
  ( NewProjectDescription (..),
    obtainNewProjectDescription,
  )
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.FeaturedStarterTemplates (getFeaturedStarterTemplates)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.GhRepo (createProjectOnDiskFromGhRepoTemplate)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Local (createProjectOnDiskFromLocalTemplate)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.StarterTemplate
  ( StarterTemplate (..),
    WaspAppAiGenerator (WaspAI),
    getTemplateName,
    getTemplateStartingInstructions,
  )
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Message as Msg
import qualified Wasp.Util.Terminal as Term

-- | It receives all of the arguments that were passed to the `wasp new` command.
createNewProject :: Arguments -> Command ()
createNewProject args = do
  newProjectArgs <- parseNewProjectArgs args & either Common.throwProjectCreationError return
  let featuredStarterTemplates = getFeaturedStarterTemplates
  newProjectDescription <- obtainNewProjectDescription newProjectArgs featuredStarterTemplates

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
    cliSendMessageC $ Msg.Start $ "Creating your project from the \"" ++ getTemplateName template ++ "\" template..."
    case template of
      GhRepoStarterTemplate ghRepoRef tmplDirPath _ ->
        createProjectOnDiskFromGhRepoTemplate absWaspProjectDir projectName appName ghRepoRef tmplDirPath
      LocalStarterTemplate tmplDirPath _ ->
        liftIO $ createProjectOnDiskFromLocalTemplate absWaspProjectDir projectName appName tmplDirPath
      AiGeneratedStarterTemplate waspAppAiGenerator _ ->
        case waspAppAiGenerator of
          WaspAI -> AI.createNewProjectInteractiveOnDisk absWaspProjectDir appName

-- | This function assumes that the project dir was created inside the current working directory.
printGettingStartedInstructionsForProject :: NewProjectDescription -> IO ()
printGettingStartedInstructionsForProject projectDescription = do
  let projectDirName = init . SP.toFilePath . SP.basename $ _absWaspProjectDir projectDescription
  let instructions = getTemplateStartingInstructions (_template projectDescription) projectDirName
  putStrLn $ Term.applyStyles [Term.Green] $ "Created new Wasp app in ./" ++ projectDirName ++ " directory!"
  putStrLn ""
  putStrLn instructions
