module Wasp.Cli.Command.CreateNewProject
  ( createNewProject,
  )
where

import Control.Monad (when)
import Control.Monad.Catch (bracket_)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Functor (($>))
import Data.List (intercalate)
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified StrongPath as SP
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.Environment (lookupEnv)
import Wasp.Cli.Command (Command, CommandError (..), commandError, errorMessage, getConsole, require)
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
    TemplateMetadata (..),
    getTemplateStartingInstructions,
  )
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Bundled (createProjectOnDiskFromBundledTemplate)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.GhReleaseArchive (createProjectOnDiskFromGhReleaseArchiveTemplate)
import Wasp.Cli.Command.Db.Migrate (migrateDev)
import Wasp.Cli.Command.Install (installIO)
import Wasp.Cli.Command.Message (cliSendMessageC, runTaskC)
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.Console (Console (..))
import Wasp.Cli.Console.Reporter (Reporter (..))
import Wasp.Cli.Util.Parser (withArguments)
import Wasp.Generator.DbGenerator.Common (MigrateArgs (..), defaultMigrateArgs)
import qualified Wasp.Message as Msg
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Project.Db (databaseUrlEnvVarName)
import Wasp.Util (indent)
import Wasp.Util.IO.Process (OutputStream (..), WriteOutput)
import Wasp.Util.TextStyle (styleLiteral)

-- | It receives all of the arguments that were passed to the `wasp new` command.
createNewProject :: Arguments -> Command ()
createNewProject = withArguments newProjectArgsParser $ \args -> do
  ValidNodeAndNpm <- require
  newProjectDescription <- obtainNewProjectDescription args availableStarterTemplates
  let absWaspProjectDir = getAbsWaspProjectDir newProjectDescription
      needsMigration = newProjectDescription._template.metadata._migratesDbOnCreation
      projectDirName = styleLiteral $ getProjectDirName newProjectDescription

  console <- getConsole
  (areDepsInstalled, isDbMigrated) <-
    runTaskC
      ("Creating the " ++ projectDirName ++ " project from the " ++ styleLiteral (show newProjectDescription._template) ++ " template")
      ("Created the " ++ projectDirName ++ " project")
      ("Failed to create the " ++ projectDirName ++ " project")
      $ do
        createProjectOnDisk newProjectDescription
        -- TODO consider removing if we start doing `wasp install` automatically
        areDepsInstalled <- liftIO $ installDepsForNewProject console absWaspProjectDir

        isDbMigrated <-
          if needsMigration && areDepsInstalled
            then migrateDbForNewProject absWaspProjectDir
            else return False

        return (areDepsInstalled, isDbMigrated)

  liftIO $ printGettingStartedInstructionsForProject console.writeRawOutput newProjectDescription

  liftIO
    $ printExtraInstructionsForProject console.reporter
    $ ["wasp install" | not areDepsInstalled]
      ++ ["wasp db migrate-dev" | needsMigration && not isDbMigrated]

createProjectOnDisk :: NewProjectDescription -> Command ()
createProjectOnDisk
  newProjectDescription@(NewProjectDescription {_template = template}) =
    case template of
      GhRepoReleaseArchiveTemplate {repo = ghRepoRef, archiveName = archiveName', archivePath = archivePath'} ->
        createProjectOnDiskFromGhReleaseArchiveTemplate newProjectDescription ghRepoRef archiveName' archivePath'
      BundledStarterTemplate {bundledPath = bundledPath'} ->
        liftIO $ createProjectOnDiskFromBundledTemplate newProjectDescription bundledPath'

-- | Returns whether the dependencies got installed.
installDepsForNewProject :: Console -> SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> IO Bool
installDepsForNewProject console absWaspProjectDir =
  installIO console.writeRawOutput absWaspProjectDir >>= \case
    Right () -> return True
    Left _err -> do
      console.reporter.send $ Msg.Warning "The project was created, but dependency installation failed" ""
      return False

-- | Creates and applies the project's initial migration, so users can run the
-- app right away. Returns whether the database got migrated.
--
-- The project was already created at this point, so a failure here only results
-- in a warning.
migrateDbForNewProject :: SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> Command Bool
migrateDbForNewProject absWaspProjectDir =
  (throwIfDbUrlInEnvironment >> inProjectDir (migrateDev initialMigrateArgs) $> True)
    `catchError` warnAboutFailedMigration
  where
    initialMigrateArgs = defaultMigrateArgs {_migrationName = Just "initial-migration"}

    -- We leave the database alone if the user's environment points us to a specific
    -- one, since it might not be a throwaway dev database.
    throwIfDbUrlInEnvironment = do
      isDbUrlInEnvironment <- liftIO $ isJust <$> lookupEnv databaseUrlEnvVarName
      when isDbUrlInEnvironment
        $ throwError
        $ commandError
          "Migration skipped"
          (styleLiteral databaseUrlEnvVarName ++ " is set in your environment, and that database might not be a throwaway dev database.")

    -- Commands find the project they work on through the current directory, and
    -- `wasp new` runs from outside of the project.
    inProjectDir action = do
      originalDir <- liftIO getCurrentDirectory
      bracket_
        (liftIO $ setCurrentDirectory $ SP.fromAbsDir absWaspProjectDir)
        (liftIO $ setCurrentDirectory originalDir)
        action

    warnAboutFailedMigration migrationError = do
      cliSendMessageC
        $ Msg.Warning "The project was created, but its database wasn't migrated"
        $ _headline migrationError ++ ": " ++ errorMessage migrationError
      return False

-- | This function assumes that the project dir was created inside the current working directory.
printGettingStartedInstructionsForProject :: WriteOutput -> NewProjectDescription -> IO ()
printGettingStartedInstructionsForProject writeRawOutput projectDescription = do
  let instructions = getTemplateStartingInstructions (getProjectDirName projectDescription) $ _template projectDescription
  writeRawOutput Stdout . T.pack $ instructions ++ "\n"

getProjectDirName :: NewProjectDescription -> String
getProjectDirName = init . SP.toFilePath . SP.basename . _absTemplateOutputDir

printExtraInstructionsForProject :: Reporter -> [String] -> IO ()
printExtraInstructionsForProject _ [] = return ()
printExtraInstructionsForProject reporter steps =
  reporter.send
    $ Msg.Warning "Some setup steps were skipped"
    $ "Due to warnings during project creation, you need to run the following setup steps for your project manually:\n"
      ++ indent 2 (intercalate "\n" $ map styleLiteral steps)
