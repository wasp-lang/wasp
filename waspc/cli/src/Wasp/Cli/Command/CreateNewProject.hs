module Wasp.Cli.Command.CreateNewProject
  ( createNewProject,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified StrongPath as SP
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Call (Arguments)
import qualified Wasp.Cli.Command.CreateNewProject.AI as AI
import Wasp.Cli.Command.CreateNewProject.ArgumentsParser (newProjectArgsParser)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (availableStarterTemplates)
import Wasp.Cli.Command.CreateNewProject.ProjectDescription
  ( NewProjectDescription (..),
    obtainNewProjectDescription,
  )
import Wasp.Cli.Command.CreateNewProject.StarterTemplates
  ( StarterTemplate (..),
    getTemplateStartingInstructions,
  )
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Bundled (createProjectOnDiskFromBundledTemplate)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.GhReleaseArchive (createProjectOnDiskFromGhReleaseArchiveTemplate)
import Wasp.Cli.Command.Install (installIO)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Util.Parser (withArguments)
import qualified Wasp.Message as Msg
import Wasp.Project.Common (WaspProjectDir)
import qualified Wasp.Util.Terminal as Term

-- | It receives all of the arguments that were passed to the `wasp new` command.
createNewProject :: Arguments -> Command ()
createNewProject = withArguments "wasp new" newProjectArgsParser $ \args -> do
  newProjectDescription <- obtainNewProjectDescription args availableStarterTemplates

  createProjectOnDisk newProjectDescription
  liftIO $ installDepsForNewProject (_absWaspProjectDir newProjectDescription)
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
      GhRepoReleaseArchiveTemplate {repo = ghRepoRef, archiveName = archiveName', archivePath = archivePath'} ->
        createProjectOnDiskFromGhReleaseArchiveTemplate (show template) absWaspProjectDir projectName appName ghRepoRef archiveName' archivePath'
      BundledStarterTemplate {bundledPath = bundledPath'} ->
        liftIO $ createProjectOnDiskFromBundledTemplate absWaspProjectDir projectName appName bundledPath'
      AiGeneratedStarterTemplate ->
        AI.createNewProjectInteractiveOnDisk absWaspProjectDir appName

-- | Runs install logic (copy wasp-config + npm install) for a newly created project.
-- If npm install fails, the project is still created — we just print a warning.
installDepsForNewProject :: SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> IO ()
installDepsForNewProject absWaspProjectDir =
  installIO absWaspProjectDir >>= \case
    Left _err ->
      putStrLn $
        Term.applyStyles [Term.Yellow] $
          "Warning: Project created, but dependency installation failed.\n"
            ++ "Run `wasp install` in the project directory to install dependencies."
    Right () -> return ()

-- | This function assumes that the project dir was created inside the current working directory.
printGettingStartedInstructionsForProject :: NewProjectDescription -> IO ()
printGettingStartedInstructionsForProject projectDescription = do
  let projectDirName = init . SP.toFilePath . SP.basename $ _absWaspProjectDir projectDescription
  let instructions = getTemplateStartingInstructions projectDirName $ _template projectDescription
  putStrLn $ Term.applyStyles [Term.Green] $ "Created new Wasp app in ./" ++ projectDirName ++ " directory!"
  putStrLn ""
  putStrLn instructions
