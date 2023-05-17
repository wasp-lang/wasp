module Wasp.Cli.Command.CreateNewProject
  ( createNewProject,
    parseNew,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Options.Applicative as O
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Call (Call (New), NewArgs (..))
import Wasp.Cli.Command.CreateNewProject.ProjectDescription
  ( NewProjectDescription (..),
    obtainNewProjectDescription,
  )
import Wasp.Cli.Command.CreateNewProject.StarterTemplates
  ( StarterTemplateName (..),
    getStarterTemplateNames,
  )
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Local (createProjectOnDiskFromLocalTemplate)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Remote (createProjectOnDiskFromRemoteTemplate)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Common (WaspProjectDir)
import qualified Wasp.Message as Msg
import qualified Wasp.Util.Terminal as Term

parseNew :: O.Parser Call
parseNew = New <$> parseNewArgs

parseNewArgs :: O.Parser NewArgs
parseNewArgs =
  NewArgs
    <$> O.optional parseProjectName
    <*> O.optional parseTemplateName

parseProjectName :: O.Parser String
parseProjectName = O.strArgument $ O.metavar "PROJECT_NAME"

parseTemplateName :: O.Parser String
parseTemplateName =
  O.strOption $
    O.long "template"
      <> O.short 't'
      <> O.metavar "TEMPLATE_NAME"
      <> O.help "Template to use for the new project"

createNewProject :: NewArgs -> Command ()
createNewProject newProjectArgs = do
  starterTemplateNames <- liftIO getStarterTemplateNames

  newProjectDescription <- obtainNewProjectDescription newProjectArgs starterTemplateNames

  createProjectOnDisk newProjectDescription
  liftIO $ printGettingStartedInstructions $ _absWaspProjectDir newProjectDescription
  where
    -- This function assumes that the project dir is created inside the current working directory when it
    -- prints the instructions.
    printGettingStartedInstructions :: Path' Abs (Dir WaspProjectDir) -> IO ()
    printGettingStartedInstructions absProjectDir = do
      let projectFolder = init . SP.toFilePath . SP.basename $ absProjectDir

      {- ORMOLU_ENABLE -}
      putStrLn $ Term.applyStyles [Term.Green] $ "Created new Wasp app in ./" ++ projectFolder ++ " directory!"
      putStrLn "To run it, do:"
      putStrLn ""
      putStrLn $ Term.applyStyles [Term.Bold] $ "    cd " ++ projectFolder
      putStrLn $ Term.applyStyles [Term.Bold] "    wasp start"

{- ORMOLU_DISABLE -}

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
      RemoteStarterTemplate remoteTemplateName ->
        createProjectOnDiskFromRemoteTemplate absWaspProjectDir projectName appName remoteTemplateName
      LocalStarterTemplate localTemplateName ->
        liftIO $ createProjectOnDiskFromLocalTemplate absWaspProjectDir projectName appName localTemplateName
