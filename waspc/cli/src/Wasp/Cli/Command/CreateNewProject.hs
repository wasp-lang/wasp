module Wasp.Cli.Command.CreateNewProject
  ( createNewProject,
  )
where

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
import Wasp.Cli.Command.Install (installIO)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import qualified Wasp.Cli.Interactive as Interactive
import Wasp.Cli.Message (cliSendMessage)
import Wasp.Cli.Util.Parser (withArguments)
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
  liftIO $ printGettingStartedInstructionsForProject areDepsInstalled newProjectDescription

createProjectOnDisk :: NewProjectDescription -> Command ()
createProjectOnDisk
  newProjectDescription@(NewProjectDescription {_template = template}) = do
    cliSendMessageC $ Msg.Start $ "Creating your project from the \"" ++ show template ++ "\" template..."
    case template of
      GhRepoReleaseArchiveTemplate {repo = ghRepoRef, archiveName = archiveName', archivePath = archivePath'} ->
        createProjectOnDiskFromGhReleaseArchiveTemplate newProjectDescription ghRepoRef archiveName' archivePath'
      BundledStarterTemplate {bundledPath = bundledPath'} ->
        liftIO $ createProjectOnDiskFromBundledTemplate newProjectDescription bundledPath'

-- | Installs the project's dependencies if the user asked for it, through the
-- flags or the question we ask in an interactive terminal. Returns whether the
-- dependencies got installed.
--
-- Without an interactive terminal (CI, scripts) we install unless told
-- otherwise, which is what scripts relying on `wasp new` expect.
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
        Just terminal -> Interactive.askYesNo terminal "Install the dependencies now?"
        Nothing -> return True

    installDepsForNewProject :: IO Bool
    installDepsForNewProject =
      installIO absWaspProjectDir >>= \case
        Right () -> return True
        Left _err -> do
          putStrLn
            $ Term.applyStyles [Term.Yellow]
            $ "Warning: The project was created, but dependency installation failed.\n"
              ++ "Run "
              ++ styleCode "wasp install"
              ++ " in the project directory to install the dependencies."
          return False

-- | This function assumes that the project dir was created inside the current working directory.
printGettingStartedInstructionsForProject :: Bool -> NewProjectDescription -> IO ()
printGettingStartedInstructionsForProject areDepsInstalled projectDescription = do
  let projectDirName = init . SP.toFilePath . SP.basename $ _absTemplateOutputDir projectDescription
  let instructions = getTemplateStartingInstructions projectDirName $ _template projectDescription
  cliSendMessage $ Msg.Success $ "Created a new Wasp app in `./" ++ projectDirName ++ "`."
  unless areDepsInstalled
    $ putStrLn
    $ "The dependencies are not installed. Run " ++ styleCode "wasp install" ++ " in `./" ++ projectDirName ++ "` before starting the app.\n"
  putStrLn instructions
