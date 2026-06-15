module Wasp.Cli.Command.CreateNewProject
  ( createNewProject,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified StrongPath as SP
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.CreateNewProject.ArgumentsParser (newProjectArgsParser)
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
import Wasp.Cli.Command.Require (NodeAndNpmInstalled (NodeAndNpmInstalled), require)
import Wasp.Cli.Message (cliSendMessage)
import Wasp.Cli.Util.Parser (withArguments)
import qualified Wasp.Message as Msg
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Util.Terminal (styleCode)
import qualified Wasp.Util.Terminal as Term

-- | It receives all of the arguments that were passed to the `wasp new` command.
createNewProject :: Arguments -> Command ()
createNewProject = withArguments "wasp new" newProjectArgsParser $ \args -> do
  NodeAndNpmInstalled <- require
  newProjectDescription <- obtainNewProjectDescription args availableStarterTemplates

  createProjectOnDisk newProjectDescription
  -- TODO consider removing if we start doing `wasp install` automatically
  liftIO $ installDepsForNewProject (getAbsWaspProjectDir newProjectDescription)
  liftIO $ printGettingStartedInstructionsForProject newProjectDescription

createProjectOnDisk :: NewProjectDescription -> Command ()
createProjectOnDisk
  newProjectDescription@(NewProjectDescription {_template = template}) = do
    cliSendMessageC $ Msg.Start $ "Creating your project from the \"" ++ show template ++ "\" template..."
    case template of
      GhRepoReleaseArchiveTemplate {repo = ghRepoRef, archiveName = archiveName', archivePath = archivePath'} ->
        createProjectOnDiskFromGhReleaseArchiveTemplate newProjectDescription ghRepoRef archiveName' archivePath'
      BundledStarterTemplate {bundledPath = bundledPath'} ->
        liftIO $ createProjectOnDiskFromBundledTemplate newProjectDescription bundledPath'

installDepsForNewProject :: SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> IO ()
installDepsForNewProject absWaspProjectDir =
  installIO absWaspProjectDir >>= \case
    Right () -> return ()
    Left _err ->
      putStrLn $
        Term.applyStyles [Term.Yellow] $
          "Warning: Project created, but dependency installation failed.\n"
            ++ "Run "
            ++ styleCode "wasp install"
            ++ " in the project directory to install dependencies."

-- | This function assumes that the project dir was created inside the current working directory.
printGettingStartedInstructionsForProject :: NewProjectDescription -> IO ()
printGettingStartedInstructionsForProject projectDescription = do
  let projectDirName = init . SP.toFilePath . SP.basename $ _absTemplateOutputDir projectDescription
  let instructions = getTemplateStartingInstructions projectDirName $ _template projectDescription
  cliSendMessage $ Msg.Success $ "Created a new Wasp app in ./" ++ projectDirName ++ " directory!"
  putStrLn instructions
