module Tests.WaspDbMigrateDevTest (waspDbMigrateDevTest) where

import Control.Monad.Reader (MonadReader (ask))
import qualified Data.Text as T
import NeatInterpolation (trimming)
import ShellCommands (ShellCommand, ShellCommandBuilder, WaspProjectContext (..), appendToPrismaFile, createTestWaspProject, inTestWaspProjectDir, waspCliDbMigrateDev, (~&&), (~|))
import StrongPath (fromAbsDir, (</>))
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
        (return [waspCliDbMigrateDevFails]),
      TestCase
        "succeed-migrations-up-to-date"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ waspCliDbMigrateDev "no_migration"
                ]
            ]
        ),
      TestCase
        "succeed-create-new-migration"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ appendToPrismaFile taskPrismaModel,
                  waspCliDbMigrateDev "yes_migration",
                  assertMigrationDirsExist "yes_migration"
                ]
            ]
        ),
      TestCase
        "succeed-with-stdin-open-but-empty"
        -- Wasp's DB connection check runs `prisma db execute --stdin`, which
        -- reads its stdin to the end. It used to inherit Wasp's stdin, so when
        -- that was a pipe nobody wrote to or closed, the check hung forever.
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ withStdinOpenButEmpty <$> waspCliDbMigrateDev "no_migration",
                  return assertStdinWasNotClosedByTimeout
                ]
            ]
        )
    ]
  where
    -- Pipes into the command from a writer that never writes anything and
    -- keeps the pipe open until the command finishes. After ~5 minutes it gives
    -- up and closes the pipe (so a hanging command can finish and the test
    -- doesn't hang), leaving a marker file behind for us to check.
    withStdinOpenButEmpty :: ShellCommand -> ShellCommand
    withStdinOpenButEmpty command =
      ( "( i=0; until [ -f "
          ++ commandFinishedMarkerFile
          ++ " ]; do i=$((i+1)); if [ \"$i\" -ge 1500 ]; then touch "
          ++ stdinTimedOutMarkerFile
          ++ "; exit 0; fi; sleep 0.2; done )"
      )
        ~| ("{ " ++ command ++ "; exitCode=$?; touch " ++ commandFinishedMarkerFile ++ "; [ \"$exitCode\" -eq 0 ]; }")

    assertStdinWasNotClosedByTimeout :: ShellCommand
    assertStdinWasNotClosedByTimeout = "[ ! -f " ++ stdinTimedOutMarkerFile ++ " ]"

    commandFinishedMarkerFile :: FilePath
    commandFinishedMarkerFile = ".wasp-e2e-command-finished"

    stdinTimedOutMarkerFile :: FilePath
    stdinTimedOutMarkerFile = ".wasp-e2e-stdin-timed-out"

    waspCliDbMigrateDevFails :: ShellCommand
    waspCliDbMigrateDevFails = "! $WASP_CLI_CMD db migrate-dev"

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
  let waspMigrationsDir = waspProjectContext.waspProjectDir </> dbMigrationsDirInWaspProjectDir
      waspOutMigrationsDir =
        waspProjectContext.waspProjectDir
          </> dotWaspDirInWaspProjectDir
          </> generatedAppDirInDotWaspDir
          </> dbRootDirInGeneratedAppDir
          </> dbMigrationsDirInDbRootDir
  return $
    ("cd " ++ fromAbsDir waspMigrationsDir)
      ~&& ("[ -d \"$(find . -type d -name '*" ++ migrationName ++ "*' -print -quit)\" ]")
      ~&& ("cd " ++ fromAbsDir waspOutMigrationsDir)
      ~&& ("[ -d \"$(find . -type d -name '*" ++ migrationName ++ "*' -print -quit)\" ]")
