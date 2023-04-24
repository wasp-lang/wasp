module Wasp.Cli.Command.CreateNewProject
  ( createNewProject,
  )
where

import Control.Monad.IO.Class (liftIO)
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.CreateNewProject.ArgumentsParser (parseNewProjectArgs)
import Wasp.Cli.Command.CreateNewProject.ProjectDescription
  ( NewProjectDescription (..),
    createNewProjectDescription,
  )
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Common
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
createNewProject argumentsList = do
  newProjectArgs <- parseNewProjectArgs argumentsList
  templateNamesFetchResult <- liftIO getStarterTemplateNames

  newProjectDescription <- createNewProjectDescription newProjectArgs templateNamesFetchResult

  createProjectOnDisk newProjectDescription
  liftIO $ printGettingStartedInstructions $ _projectName newProjectDescription
  where
    printGettingStartedInstructions :: String -> IO ()
    printGettingStartedInstructions projectFolder = do
      putStrLn $ Term.applyStyles [Term.Green] ("Created new Wasp app in ./" ++ projectFolder ++ " directory!")
      putStrLn "To run it, do:"
      putStrLn ""
      putStrLn $ Term.applyStyles [Term.Bold] ("    cd " ++ projectFolder)
      putStrLn $ Term.applyStyles [Term.Bold] "    wasp start"

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
