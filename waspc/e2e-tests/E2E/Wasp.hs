-- | Steps for working with the Wasp CLI and Wasp projects.
module E2E.Wasp
  ( waspAppDirName,
    wasp,
    waspFails,
    waspFailsOutsideProject,
    assertFailsWithoutInteractiveTerminal,
    waspCliNew,
    createWaspProject,
    withNewWaspProject,
    getProjectDirAndAppName,
    waspDbMigrateDev,
    waspCliDbResetForce,
    dbMigrationsDirs,
    listDbMigrations,
    setDbToPostgres,
    getWaspPidFromLockFile,
    textWaspVersion,
  )
where

import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import Data.List (isSuffixOf)
import qualified Data.Text as T
import E2E.Assertions (assertOutputContains)
import E2E.Command (Command, waspCli, withEnv)
import E2E.Files (replaceInFile)
import E2E.Process (ProcessResult, run, runExpectingFailure)
import E2E.TestM (TestM, failTest, getWorkingDir, inDirectory, resolvePath)
import qualified StrongPath as SP
import System.Directory (canonicalizePath, doesDirectoryExist, listDirectory, renameDirectory)
import System.FilePath ((</>))
import Wasp.Cli.Command.CreateNewProject.ProjectDescription (parseWaspProjectNameIntoAppName)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates (StarterTemplate)
import Wasp.Cli.ProjectLock.Data (ProjectLockData (..))
import Wasp.Cli.ProjectLock.Handle (projectLockFileInWaspProjectDir)
import Wasp.Generator.DbGenerator.Common (dbMigrationsDirInDbRootDir, dbRootDirInGeneratedAppDir)
import Wasp.Project.Common (WaspProjectDir, generatedAppDirInWaspProjectDir)
import Wasp.Project.Db.Migrations (dbMigrationsDirInWaspProjectDir)
import Wasp.Version (waspVersion)

-- | The directory (and the name) of the Wasp project a test creates.
waspAppDirName :: FilePath
waspAppDirName = "wasp-app"

-- | Runs a Wasp CLI command and fails the test if it doesn't succeed.
wasp :: [String] -> TestM ProcessResult
wasp = run . waspCli

-- | Runs a Wasp CLI command and fails the test if it succeeds.
waspFails :: [String] -> TestM ProcessResult
waspFails = runExpectingFailure . waspCli

-- | Runs a Wasp CLI command that needs a Wasp project in a dir that isn't in
-- one, and checks that it fails for that reason.
waspFailsOutsideProject :: [String] -> TestM ()
waspFailsOutsideProject arguments =
  waspFails arguments >>= assertOutputContains "Couldn't find wasp project root"

-- | Asserts that a command that needs to prompt fails when stdin is not an
-- interactive terminal, and prints its usage so the user can pass the answers
-- as arguments instead.
assertFailsWithoutInteractiveTerminal :: String -> Command -> TestM ()
assertFailsWithoutInteractiveTerminal commandName cmd = do
  result <- runExpectingFailure cmd
  assertOutputContains "stdin is not an interactive terminal" result
  assertOutputContains (T.pack $ "Usage: " ++ commandName) result

-- | Creates the Wasp project in 'waspAppDirName'.
waspCliNew :: StarterTemplate -> Command
waspCliNew template = waspCli ["new", waspAppDirName, "-t", show template]

createWaspProject :: StarterTemplate -> TestM ()
createWaspProject = void . run . waspCliNew

-- | Creates a Wasp project and runs the steps inside of it.
withNewWaspProject :: StarterTemplate -> TestM a -> TestM a
withNewWaspProject template steps = createWaspProject template >> inDirectory waspAppDirName steps

-- | The project dir and app name Wasp derives the names of the project's
-- resources from (e.g. Docker containers), so that a test can derive them the
-- same way, before anything creates them. For the project
-- 'withNewWaspProject' created, run in its dir.
getProjectDirAndAppName :: TestM (SP.Path' SP.Abs (SP.Dir WaspProjectDir), String)
getProjectDirAndAppName = do
  workingDir <- getWorkingDir
  -- Wasp finds the project dir through the OS's current dir, which has no
  -- symlinks.
  projectDir <- liftIO $ canonicalizePath workingDir >>= SP.parseAbsDir
  -- `wasp new` names the app after the project's dir.
  appName <- case parseWaspProjectNameIntoAppName waspAppDirName of
    Left err -> failTest $ "Couldn't get the app name of the project: " ++ err
    Right name -> return $ show name
  return (projectDir, appName)

-- | Runs `wasp db migrate-dev` in the Wasp project.
--
-- We make the migration name deterministic by renaming its directory to
-- @no-date-<migrationName>@, instead of the usual @<date>_<migrationName>@.
-- Snapshots shouldn't change with every run.
-- Caveat: a project can't have two migrations with the same name.
waspDbMigrateDev :: String -> TestM ()
waspDbMigrateDev migrationName = do
  wasp ["db", "migrate-dev", "--name", migrationName] >>= assertOutputContains "Database successfully migrated."
  forM_ dbMigrationsDirs $ \migrationsDir -> do
    migrations <- listDbMigrations migrationsDir
    absMigrationsDir <- resolvePath migrationsDir
    -- When there is nothing to migrate, `wasp db migrate-dev` creates no
    -- migration, so there's nothing to rename.
    forM_ (filter (("_" ++ migrationName) `isSuffixOf`) migrations) $ \migration ->
      liftIO $ renameDirectory (absMigrationsDir </> migration) (absMigrationsDir </> ("no-date-" ++ migrationName))

-- | The migrations dir in the Wasp project and its copy in the generated app,
-- relative to the Wasp project dir.
dbMigrationsDirs :: [FilePath]
dbMigrationsDirs =
  [ SP.fromRelDir dbMigrationsDirInWaspProjectDir,
    SP.fromRelDir $ generatedAppDirInWaspProjectDir SP.</> dbRootDirInGeneratedAppDir SP.</> dbMigrationsDirInDbRootDir
  ]

-- | The names of the migrations in one of 'dbMigrationsDirs'. A project
-- without migrations might not have the dir.
listDbMigrations :: FilePath -> TestM [FilePath]
listDbMigrations migrationsDir = do
  absMigrationsDir <- resolvePath migrationsDir
  liftIO $ do
    exists <- doesDirectoryExist absMigrationsDir
    if exists then listDirectory absMigrationsDir else return []

-- | Prisma refuses to run `migrate reset` when it detects an AI agent (e.g.
-- when the e2e tests are run from Claude Code), unless the consent env var is
-- set. Tests only reset their own throwaway database, so we always consent.
waspCliDbResetForce :: Command
waspCliDbResetForce =
  withEnv [("PRISMA_USER_CONSENT_FOR_DANGEROUS_AI_ACTION", "Resetting the throwaway e2e test database")] $
    waspCli ["db", "reset", "--force"]

setDbToPostgres :: TestM ()
setDbToPostgres = replaceInFile "schema.prisma" "provider = \"sqlite\"" "provider = \"postgresql\""

-- | The PID of the Wasp process that holds the project lock (or held it last).
--
-- We signal this PID instead of the one of the process we started, since the
-- Wasp CLI might run through a wrapper (e.g. @tools/wasp-cli-dev@).
getWaspPidFromLockFile :: TestM Int
getWaspPidFromLockFile = do
  lockFile <- resolvePath $ SP.fromRelFile projectLockFileInWaspProjectDir
  liftIO (Aeson.eitherDecodeFileStrict lockFile) >>= \case
    Right (lockData :: ProjectLockData) -> return $ fromIntegral lockData.pid
    Left err -> failTest $ "Couldn't read the Wasp PID from " ++ lockFile ++ ": " ++ err

-- | For use in @main.wasp.ts@ files.
textWaspVersion :: T.Text
textWaspVersion = T.pack $ show waspVersion
