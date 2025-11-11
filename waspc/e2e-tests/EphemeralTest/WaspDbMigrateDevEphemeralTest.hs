module EphemeralTest.WaspDbMigrateDevEphemeralTest (waspDbMigrateDevEphemeralTest) where

import Control.Monad.Reader (MonadReader (ask))
import EphemeralTest (EphemeralTest, makeEphemeralTest, makeEphemeralTestCase)
import EphemeralTest.ShellCommands (createEphemeralWaspProject, withInEphemeralWaspProjectDir)
import ShellCommands (ShellCommand, ShellCommandBuilder, WaspNewTemplate (..), (~&&))
import StrongPath (fromAbsDir, (</>))
import Wasp.Generator.DbGenerator.Common
  ( dbMigrationsDirInDbRootDir,
    dbRootDirInProjectRootDir,
  )
import Wasp.Project.Common
  ( dotWaspDirInWaspProjectDir,
    generatedCodeDirInDotWaspDir,
  )
import Wasp.Project.Db.Migrations (dbMigrationsDirInWaspProjectDir)
import WaspProject.ShellCommands (WaspProjectContext (..), appendToPrismaFile, waspCliDbMigrateDevDev)

-- | TODO: Test on all databases (e.g. Postgresql)
waspDbMigrateDevEphemeralTest :: EphemeralTest
waspDbMigrateDevEphemeralTest =
  makeEphemeralTest
    "wasp-db-migrate-dev"
    [ makeEphemeralTestCase
        "Should fail outside of a Wasp project"
        waspCliDbMigrateDevFails,
      makeEphemeralTestCase
        "Setup: Create Wasp project from minimal starter"
        (createEphemeralWaspProject Minimal),
      makeEphemeralTestCase
        "Should succeed when migrations up to date inside of a Wasp project"
        (withInEphemeralWaspProjectDir [waspCliDbMigrateDevDev "no_migration"]),
      makeEphemeralTestCase
        "Setup: Add a Task model to prisma"
        ( withInEphemeralWaspProjectDir
            [appendToPrismaFile taskPrismaModel]
        ),
      makeEphemeralTestCase
        "Should succeed creating a new migration inside of a Wasp project"
        (withInEphemeralWaspProjectDir [waspCliDbMigrateDevDev "yes_migration"]),
      makeEphemeralTestCase
        "Assert migration directories exists"
        (withInEphemeralWaspProjectDir [assertMigrationDirsExist "yes_migration"])
    ]
  where
    waspCliDbMigrateDevFails :: ShellCommandBuilder context ShellCommand
    waspCliDbMigrateDevFails = return "! wasp-cli db migrate-dev"

    taskPrismaModel =
      unlines
        [ "model Task {",
          "  id          Int     @id @default(autoincrement())",
          "  description String",
          "  isDone      Boolean @default(false)",
          "}"
        ]

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
    "cd "
      ++ fromAbsDir waspMigrationsDir
        ~&& "[ -d \"$(find . -type d -name '*"
      ++ migrationName
      ++ "*' -print -quit)\" ]"
        ~&& "cd "
      ++ fromAbsDir waspOutMigrationsDir
        ~&& "[ -d \"$(find . -type d -name '*"
      ++ migrationName
      ++ "*' -print -quit)\" ]"
