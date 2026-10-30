module Wasp.Cli.Command.CreateNewProject
  ( createNewProject,
  )
where

import Control.Concurrent (newChan)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import qualified StrongPath as SP
import Wasp.Cli.Command (Command, require)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.CreateNewProject.ArgumentsParser (NewProjectArgs (..), newProjectArgsParser)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (availableStarterTemplates)
import Wasp.Cli.Command.CreateNewProject.ProjectDescription
  ( NewProjectDescription (..),
    getAbsWaspProjectDir,
    obtainNewProjectDescription,
  )
import Wasp.Cli.Command.CreateNewProject.StarterTemplates
  ( StarterTemplate (..),
    getTemplateStartingInstructions,
  )
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Bundled (createProjectOnDiskFromBundledTemplate)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.GhReleaseArchive (createProjectOnDiskFromGhReleaseArchiveTemplate)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import qualified Wasp.Cli.Interactive as Interactive
import Wasp.Cli.Message (cliSendMessage)
import Wasp.Cli.Util.Parser (withArguments)
import Wasp.Generator.NpmInstall (installProjectNpmDependencies)
import qualified Wasp.Message as Msg
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Util.Terminal (styleCode)
import qualified Wasp.Util.Terminal as Term

-- | It receives all of the arguments that were passed to the `wasp new` command.
createNewProject :: Arguments -> Command ()
createNewProject = withArguments newProjectArgsParser $ \args -> do
  ValidNodeAndNpm <- require
  newProjectDescription <- obtainNewProjectDescription args availableStarterTemplates

  createProjectOnDisk newProjectDescription
  areDepsInstalled <- liftIO $ installDepsForNewProjectIfWanted (_shouldInstallDeps args) (getAbsWaspProjectDir newProjectDescription)
  liftIO $ printGettingStartedInstructionsForProject newProjectDescription
  unless areDepsInstalled $ liftIO $ printInstallDepsReminder newProjectDescription

createProjectOnDisk :: NewProjectDescription -> Command ()
createProjectOnDisk
  newProjectDescription@(NewProjectDescription {_template = template}) = do
    cliSendMessageC $ Msg.Start $ "Creating your project from the \"" ++ show template ++ "\" template..."
    case template of
      GhRepoReleaseArchiveTemplate {repo = ghRepoRef, archiveName = archiveName', archivePath = archivePath'} ->
        createProjectOnDiskFromGhReleaseArchiveTemplate newProjectDescription ghRepoRef archiveName' archivePath'
      BundledStarterTemplate {bundledPath = bundledPath'} ->
        liftIO $ createProjectOnDiskFromBundledTemplate newProjectDescription bundledPath'

-- | Installs the project's npm dependencies if the user asked for it (through
-- the flags, or the question we ask in an interactive shell). Returns whether
-- the dependencies got installed.
--
-- When not interactive (CI, scripts), we install unless told otherwise, since
-- that is what every script relying on `wasp new` expects.
installDepsForNewProjectIfWanted :: Maybe Bool -> SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> IO Bool
installDepsForNewProjectIfWanted shouldInstallDepsArg absWaspProjectDir = do
  shouldInstallDeps <- maybe askIfInteractive return shouldInstallDepsArg
  if shouldInstallDeps
    then installDepsForNewProject
    else return False
  where
    askIfInteractive :: IO Bool
    askIfInteractive =
      Interactive.getInteractiveTerminal >>= \case
        Just terminal -> Interactive.askYesNo terminal "Install the npm dependencies now?" Interactive.DefaultYes
        Nothing -> return True

    installDepsForNewProject :: IO Bool
    installDepsForNewProject = do
      messagesChan <- newChan
      installProjectNpmDependencies messagesChan absWaspProjectDir >>= \case
        Right () -> return True
        Left _err -> do
          putStrLn
            $ Term.applyStyles [Term.Yellow]
            $ "Warning: The project was created, but dependency installation failed.\n"
              ++ "Run "
              ++ styleCode "npm install"
              ++ " in the project directory to install the dependencies."
          return False

-- | This function assumes that the project dir was created inside the current working directory.
printGettingStartedInstructionsForProject :: NewProjectDescription -> IO ()
printGettingStartedInstructionsForProject projectDescription = do
  let projectDirName = getProjectDirName projectDescription
  let instructions = getTemplateStartingInstructions projectDirName $ _template projectDescription
  cliSendMessage $ Msg.Success $ "Created a new Wasp app in `./" ++ projectDirName ++ "`."
  putStrLn instructions

printInstallDepsReminder :: NewProjectDescription -> IO ()
printInstallDepsReminder projectDescription =
  putStrLn $ "Run " ++ styleCode "npm install" ++ " in `./" ++ getProjectDirName projectDescription ++ "` before starting the app."

getProjectDirName :: NewProjectDescription -> String
getProjectDirName = init . SP.toFilePath . SP.basename . _absTemplateOutputDir
