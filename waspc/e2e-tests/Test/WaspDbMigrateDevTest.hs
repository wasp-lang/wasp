module Test.WaspDbMigrateDevTest (waspDbMigrateDevTest) where

import Control.Monad.Reader (MonadReader (ask))
import qualified Data.Text as T
import NeatInterpolation (trimming)
import ShellCommands (ShellCommand, ShellCommandBuilder, WaspNewTemplate (..), (~&&))
import StrongPath (fromAbsDir, (</>))
import Test (Test, makeTest, makeTestCase)
import Test.ShellCommands (createTestWaspProject, withInTestWaspProjectDir)
import Wasp.Generator.DbGenerator.Common
  ( dbMigrationsDirInDbRootDir,
    dbRootDirInProjectRootDir,
  )
import Wasp.Project.Common
  ( dotWaspDirInWaspProjectDir,
    generatedCodeDirInDotWaspDir,
  )
import Wasp.Project.Db.Migrations (dbMigrationsDirInWaspProjectDir)
import WaspProject.ShellCommands (WaspProjectContext (..), appendToPrismaFile, waspCliDbMigrateDev)

-- | TODO: Test on all databases (e.g. Postgresql)
waspDbMigrateDevTest :: Test
waspDbMigrateDevTest =
  makeTest
    "wasp-db-migrate-dev"
    [ makeTestCase
        "Should fail outside of a Wasp project"
        (return [waspCliDbMigrateDevFails]),
      makeTestCase
        "Should succeed when migrations up to date inside of a Wasp project"
        ( sequence
            [ createTestWaspProject Minimal,
              withInTestWaspProjectDir [waspCliDbMigrateDev "no_migration"]
            ]
        ),
      makeTestCase
        "Should succeed creating a new migration inside of a Wasp project"
        ( sequence
            [ createTestWaspProject Minimal,
              withInTestWaspProjectDir
                [ appendToPrismaFile taskPrismaModel,
                  waspCliDbMigrateDev "yes_migration",
                  assertMigrationDirsExist "yes_migration"
                ]
            ]
        )
    ]
  where
    waspCliDbMigrateDevFails :: ShellCommand
    waspCliDbMigrateDevFails = "! wasp-cli db migrate-dev"

    taskPrismaModel :: T.Text
    taskPrismaModel =
      [trimming|
        model Task {
          id          Int     @id @default(autoincrement())
          description String
          isDone      Boolean @default(false)
        }
      |]

assertMigrationDirsExist :: String -> ShellCommandBuilder WaspProjectContext ShellCommand
assertMigrationDirsExist migrationName = do
  waspProjectContext <- ask
  let waspMigrationsDir = _waspProjectDir waspProjectContext </> dbMigrationsDirInWaspProjectDir
      waspOutMigrationsDir =
        _waspProjectDir waspProjectContext
          </> dotWaspDirInWaspProjectDir
          </> generatedCodeDirInDotWaspDir
          </> dbRootDirInProjectRootDir
          </> dbMigrationsDirInDbRootDir
  return $
    ("cd " ++ fromAbsDir waspMigrationsDir)
      ~&& ("[ -d \"$(find . -type d -name '*" ++ migrationName ++ "*' -print -quit)\" ]")
      ~&& ("cd " ++ fromAbsDir waspOutMigrationsDir)
      ~&& ("[ -d \"$(find . -type d -name '*" ++ migrationName ++ "*' -print -quit)\" ]")
