module Wasp.Cli.Command.CreateNewProject
  ( createNewProject,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.CreateNewProject.ArgumentsParser (parseNewProjectArgs)
import Wasp.Cli.Command.CreateNewProject.Common (throwProjectCreationError)
import Wasp.Cli.Command.CreateNewProject.ProjectDescription
  ( NewProjectDescription (..),
    obtainNewProjectDescription,
  )
import Wasp.Cli.Command.CreateNewProject.StarterTemplates
  ( StarterTemplateInfo (..),
    TemplateMetadata (..),
    getStarterTemplateInfos,
  )
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Local (createProjectOnDiskFromLocalTemplate)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Remote (createProjectOnDiskFromRemoteTemplate)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Common (WaspProjectDir)
import qualified Wasp.Message as Msg
import qualified Wasp.Util.Terminal as Term

-- It receives all of the arguments that were passed to the `wasp new` command.
createNewProject :: Arguments -> Command ()
createNewProject args = do
  newProjectArgs <- parseNewProjectArgs args & either throwProjectCreationError return
  starterTemplateInfos <- liftIO getStarterTemplateInfos

  newProjectDescription <- obtainNewProjectDescription newProjectArgs starterTemplateInfos

  createProjectOnDisk newProjectDescription
  liftIO $ printGettingStartedInstructions $ _absWaspProjectDir newProjectDescription
  where
    -- This function assumes that the project dir is created inside the current working directory when it
    -- prints the instructions.
    printGettingStartedInstructions :: Path' Abs (Dir WaspProjectDir) -> IO ()
    printGettingStartedInstructions absProjectDir = do
      let projectFolder = init . SP.toFilePath . SP.basename $ absProjectDir
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
      _templateInfo = templateInfo,
      _absWaspProjectDir = absWaspProjectDir
    } = do
    cliSendMessageC $ Msg.Start $ "Creating your project from the \"" ++ show templateInfo ++ "\" template..."
    case templateInfo of
      RemoteStarterTemplate TemplateMetadata {_path = remoteTemplatePath} ->
        createProjectOnDiskFromRemoteTemplate absWaspProjectDir projectName appName remoteTemplatePath
      LocalStarterTemplate TemplateMetadata {_path = localTemplatePath} ->
        liftIO $ createProjectOnDiskFromLocalTemplate absWaspProjectDir projectName appName localTemplatePath
