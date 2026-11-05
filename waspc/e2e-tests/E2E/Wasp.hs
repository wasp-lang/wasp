-- | Steps for working with the Wasp CLI and Wasp projects.
module E2E.Wasp
  ( waspAppDirName,
    wasp,
    waspFails,
    createWaspProject,
    withNewWaspProject,
    waspDbMigrateDev,
    setDbToPostgres,
    waspPidFromLockFile,
    textWaspVersion,
  )
where

import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import Data.List (isSuffixOf)
import qualified Data.Text as T
import E2E.Command (waspCli)
import E2E.Files (replaceInFile)
import E2E.Process (ProcessResult, run, runExpectingFailure)
import E2E.TestM (TestM, failTest, inDirectory, resolvePath)
import qualified StrongPath as SP
import System.Directory (doesDirectoryExist, listDirectory, renameDirectory)
import System.FilePath ((</>))
import Wasp.Cli.Command.CreateNewProject.StarterTemplates (StarterTemplate)
import Wasp.Cli.ProjectLock.Data (ProjectLockData (..))
import Wasp.Cli.ProjectLock.Handle (projectLockFileInWaspProjectDir)
import Wasp.Generator.DbGenerator.Common (dbMigrationsDirInDbRootDir, dbRootDirInGeneratedAppDir)
import Wasp.Project.Common (dotWaspDirInWaspProjectDir, generatedAppDirInDotWaspDir)
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

createWaspProject :: StarterTemplate -> TestM ()
createWaspProject template = void $ wasp ["new", waspAppDirName, "-t", show template]

-- | Creates a Wasp project and runs the steps inside of it.
withNewWaspProject :: StarterTemplate -> TestM a -> TestM a
withNewWaspProject template steps = createWaspProject template >> inDirectory waspAppDirName steps

-- | Runs `wasp db migrate-dev` in the Wasp project.
--
-- We make the migration name deterministic by renaming its directory to
-- @no-date-<migrationName>@, instead of the usual @<date>_<migrationName>@.
-- Snapshots shouldn't change with every run.
-- Caveat: a project can't have two migrations with the same name.
waspDbMigrateDev :: String -> TestM ()
waspDbMigrateDev migrationName = do
  void $ wasp ["db", "migrate-dev", "--name", migrationName]
  mapM resolvePath migrationsDirs >>= mapM_ removeDatePrefix
  where
    removeDatePrefix migrationsDir = liftIO $ do
      exists <- doesDirectoryExist migrationsDir
      -- When there is nothing to migrate, `wasp db migrate-dev` creates no migration.
      entries <- if exists then listDirectory migrationsDir else return []
      forM_ (filter (("_" ++ migrationName) `isSuffixOf`) entries) $ \migrationDir ->
        renameDirectory (migrationsDir </> migrationDir) (migrationsDir </> ("no-date-" ++ migrationName))

    -- The migrations dir in the project and its copy in the generated app.
    migrationsDirs =
      [ SP.fromRelDir dbMigrationsDirInWaspProjectDir,
        SP.fromRelDir $
          dotWaspDirInWaspProjectDir
            SP.</> generatedAppDirInDotWaspDir
            SP.</> dbRootDirInGeneratedAppDir
            SP.</> dbMigrationsDirInDbRootDir
      ]

setDbToPostgres :: TestM ()
setDbToPostgres = replaceInFile "schema.prisma" "provider = \"sqlite\"" "provider = \"postgresql\""

-- | The PID of the Wasp process that holds the project lock (or held it last).
--
-- We signal this PID instead of the one of the process we started, since the
-- Wasp CLI might run through a wrapper (e.g. @tools/wasp-cli-dev@).
waspPidFromLockFile :: TestM Int
waspPidFromLockFile = do
  lockFile <- resolvePath $ SP.fromRelFile projectLockFileInWaspProjectDir
  liftIO (Aeson.eitherDecodeFileStrict lockFile) >>= \case
    Right (lockData :: ProjectLockData) -> return $ fromIntegral lockData.pid
    Left err -> failTest $ "Couldn't read the Wasp PID from " ++ lockFile ++ ": " ++ err

-- | For use in @main.wasp.ts@ files.
textWaspVersion :: T.Text
textWaspVersion = T.pack $ show waspVersion
