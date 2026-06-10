module Tests.WaspDbMigrateDevTest (waspDbMigrateDevTest) where

import Context (WaspProjectContext (..))
import qualified Data.Text as T
import NeatInterpolation (trimming)
import Step (Step, askStepContext)
import Steps
  ( appendToPrismaFile,
    assertDirHasSubdirWithNameContaining,
    createTestWaspProject,
    inTestWaspProjectDir,
    runCommandExpectingFailure,
    waspCli,
    waspCliDbMigrateDev,
  )
import StrongPath ((</>))
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
    [ TestCase
        "fail-outside-project"
        [runCommandExpectingFailure $ waspCli ["db", "migrate-dev"]],
      TestCase
        "succeed-migrations-up-to-date"
        [ createTestWaspProject minimalStarterTemplate,
          inTestWaspProjectDir
            [ waspCliDbMigrateDev "no_migration"
            ]
        ],
      TestCase
        "succeed-create-new-migration"
        [ createTestWaspProject minimalStarterTemplate,
          inTestWaspProjectDir
            [ appendToPrismaFile taskPrismaModel,
              waspCliDbMigrateDev "yes_migration",
              assertMigrationDirsExist "yes_migration"
            ]
        ]
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

assertMigrationDirsExist :: String -> Step WaspProjectContext ()
assertMigrationDirsExist migrationName = do
  waspProjectContext <- askStepContext
  let waspMigrationsDir = waspProjectContext.waspProjectDir </> dbMigrationsDirInWaspProjectDir
      waspOutMigrationsDir =
        waspProjectContext.waspProjectDir
          </> dotWaspDirInWaspProjectDir
          </> generatedAppDirInDotWaspDir
          </> dbRootDirInGeneratedAppDir
          </> dbMigrationsDirInDbRootDir
  assertDirHasSubdirWithNameContaining waspMigrationsDir migrationName
  assertDirHasSubdirWithNameContaining waspOutMigrationsDir migrationName
