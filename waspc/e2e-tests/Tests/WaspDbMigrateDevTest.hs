module Tests.WaspDbMigrateDevTest (waspDbMigrateDevTest) where

import Context (WaspProjectContext)
import qualified Data.Text as T
import NeatInterpolation (trimming)
import Step (Step)
import Steps
  ( appendToPrismaFile,
    assertDirExists,
    createWaspProject,
    inWaspProjectDir,
    runCommandExpectingFailure,
    waspCli,
    waspCliDbMigrateDev,
  )
import StrongPath (fromRelDir, (</>))
import qualified System.FilePath as FP
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Generator.DbGenerator.Common
  ( dbMigrationsDirInDbRootDir,
    dbRootDirInGeneratedAppDir,
  )
import Wasp.Project.Common
  ( dotWaspDirInWaspProjectDir,
    generatedAppDirInDotWaspDir,
  )
import Wasp.Project.Db.Migrations (dbMigrationsDirInWaspProjectDir)

-- | TODO: Test on all databases (e.g. Postgresql)
waspDbMigrateDevTest :: Test
waspDbMigrateDevTest =
  Test
    "wasp-db-migrate-dev"
    [ TestCase "fail-outside-project" $
        runCommandExpectingFailure $
          waspCli ["db", "migrate-dev"],
      TestCase "succeed-migrations-up-to-date" $ do
        createWaspProject minimalStarterTemplate
        inWaspProjectDir $
          waspCliDbMigrateDev "no_migration",
      TestCase "succeed-create-new-migration" $ do
        createWaspProject minimalStarterTemplate
        inWaspProjectDir $ do
          appendToPrismaFile taskPrismaModel
          waspCliDbMigrateDev "yes_migration"
          assertMigrationDirsExist "yes_migration"
    ]
  where
    taskPrismaModel :: T.Text
    taskPrismaModel =
      [trimming|
        model Task {
          id          Int     @id @default(autoincrement())
          description String
          isDone      Boolean @default(false)
        }
      |]

-- | After 'waspCliDbMigrateDev' runs, it normalizes the migration directory
-- name to @no-date-<migrationName>@, so we assert that exact directory exists
-- (both in the Wasp project and in the generated app).
assertMigrationDirsExist :: String -> Step WaspProjectContext ()
assertMigrationDirsExist migrationName = do
  assertMigrationDirExists (fromRelDir dbMigrationsDirInWaspProjectDir)
  assertMigrationDirExists (fromRelDir waspOutMigrationsDir)
  where
    waspOutMigrationsDir =
      dotWaspDirInWaspProjectDir
        </> generatedAppDirInDotWaspDir
        </> dbRootDirInGeneratedAppDir
        </> dbMigrationsDirInDbRootDir
    assertMigrationDirExists migrationsDir =
      assertDirExists (migrationsDir FP.</> ("no-date-" ++ migrationName))
