module Wasp.Cli.Command.CreateNewProject
  ( createNewProject,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.CreateNewProject.ArgumentsParser (parseNewProjectArgs)
import Wasp.Cli.Command.CreateNewProject.Common (throwProjectCreationError)
import Wasp.Cli.Command.CreateNewProject.ProjectDescription
  ( NewProjectDescription (..),
    NewProjectName (NewProjectName),
    obtainNewProjectDescription,
  )
import Wasp.Cli.Command.CreateNewProject.StarterTemplates
  ( StarterTemplateName (..),
    getStarterTemplateNames,
  )
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Local (createProjectOnDiskFromLocalTemplate)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Remote (createProjectOnDiskFromRemoteTemplate)
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Message as Msg
import qualified Wasp.Util.Terminal as Term

-- It receives all of the arguments that were passed to the `wasp new` command.
createNewProject :: Arguments -> Command ()
createNewProject args = do
  newProjectArgs <-
    parseNewProjectArgs args
      & \case
        Right newProjectArgs -> return newProjectArgs
        Left err -> throwProjectCreationError err
  starterTemplateNames <- liftIO getStarterTemplateNames

  newProjectDescription <- obtainNewProjectDescription newProjectArgs starterTemplateNames

  createProjectOnDisk newProjectDescription
  liftIO $ printGettingStartedInstructions $ _projectName newProjectDescription
  where
    printGettingStartedInstructions :: NewProjectName -> IO ()
    printGettingStartedInstructions (NewProjectName projectName) = do
      let projectFolder = projectName
{- ORMOLU_DISABLE -}
      putStrLn $ Term.applyStyles [Term.Green] $ "Created new Wasp app in ./" ++ projectFolder ++ " directory!"
      putStrLn                                   "To run it, do:"
      putStrLn                                   ""
      putStrLn $ Term.applyStyles [Term.Bold] $  "    cd " ++ projectFolder
      putStrLn $ Term.applyStyles [Term.Bold]    "    wasp start"
{- ORMOLU_ENABLE -}

createProjectOnDisk :: NewProjectDescription -> Command ()
createProjectOnDisk
  NewProjectDescription
    { _projectName = projectName,
      _appName = appName,
      _templateName = templateName,
      _absWaspProjectDir = absWaspProjectDir
    } = do
    cliSendMessageC $ Msg.Start $ "Creating your project from the " ++ show templateName ++ " template..."
    case templateName of
      RemoteTemplate remoteTemplateName ->
        createProjectOnDiskFromRemoteTemplate absWaspProjectDir projectName appName remoteTemplateName
      LocalTemplate localTemplateName ->
        liftIO $ createProjectOnDiskFromLocalTemplate absWaspProjectDir projectName appName localTemplateName
