module Tests.WaspDbStartTest (waspDbStartTest) where

import Control.Monad.Reader (ask)
import Data.List (intercalate)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    WaspProjectContext (..),
    assertCommandOutputContains,
    createTestWaspProject,
    inTestWaspProjectDir,
    setWaspDbToPSQL,
    skipIfDockerDisabled,
    waspCliDbMigrateDev,
    waspCliDbStart,
  )
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Db.Postgres (defaultPostgresPort)
import qualified Wasp.Project.Db.Dev.Postgres as Dev.Postgres

waspDbStartTest :: Test
waspDbStartTest =
  Test
    "wasp-db-start"
    [ TestCase
        "fail-outside-project"
        (return ["! $WASP_CLI_CMD db start"]),
      TestCase
        "fail-sqlite-project"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir [assertCommandOutputContains (("! " ++) <$> waspCliDbStart) "SQLite uses a local file"]
            ]
        ),
      TestCase
        "fail-sqlite-docker-option"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ assertCommandOutputContains
                    (return "! $WASP_CLI_CMD db migrate-dev --db-image postgres:18")
                    "only apply to Wasp-managed PostgreSQL"
                ]
            ]
        ),
      TestCase
        "postgresql-command-lifecycle"
        ( skipIfDockerDisabled . sequence $
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ setWaspDbToPSQL,
                  waspCliDbMigrateDev "first_migration",
                  assertCommandOutputContains (return "$WASP_CLI_CMD db reset --force") "PostgreSQL ready.",
                  occupyDefaultDevDbPort,
                  waspCliDbMigrateDev "no_new_migration",
                  removeDefaultDevDbPortHolder,
                  assertStartFailsWhenRunning,
                  removeDevDbVolume
                ]
            ]
        )
    ]

assertStartFailsWhenRunning :: ShellCommandBuilder WaspProjectContext ShellCommand
assertStartFailsWhenRunning = do
  context <- ask
  let db = Dev.Postgres.makeDevPostgresDbSpec context.waspProjectDir minimalStarterAppName defaultPostgresPort
  return $
    intercalate
      "\n"
      [ "(",
        "set -e",
        "$WASP_CLI_CMD db start > .wasp-e2e-db-start.log 2>&1 &",
        "db_pid=$!",
        "cleanup() {",
        "  docker stop " ++ db.dockerContainerName ++ " >/dev/null 2>&1 || true",
        "  kill \"$db_pid\" 2>/dev/null || true",
        "  kill \"$second_pid\" 2>/dev/null || true",
        "}",
        "trap cleanup EXIT",
        "attempt=0",
        "until grep -q 'PostgreSQL ready' .wasp-e2e-db-start.log; do",
        "  attempt=$((attempt+1))",
        "  [ \"$attempt\" -lt 180 ]",
        "  sleep 1",
        "done",
        "attempt=0",
        "until grep -q 'database system is ready to accept connections' .wasp-e2e-db-start.log; do",
        "  attempt=$((attempt+1))",
        "  [ \"$attempt\" -lt 30 ]",
        "  sleep 1",
        "done",
        "$WASP_CLI_CMD db start > .wasp-e2e-db-second-start.log 2>&1 &",
        "second_pid=$!",
        "attempt=0",
        "while kill -0 \"$second_pid\" 2>/dev/null; do",
        "  attempt=$((attempt+1))",
        "  [ \"$attempt\" -lt 30 ]",
        "  sleep 1",
        "done",
        "if wait \"$second_pid\"",
        "then",
        "  exit 1",
        "fi",
        "grep -q 'PostgreSQL already running' .wasp-e2e-db-second-start.log",
        "grep -q 'Database URL:' .wasp-e2e-db-second-start.log",
        ")"
      ]

occupyDefaultDevDbPort :: ShellCommandBuilder WaspProjectContext ShellCommand
occupyDefaultDevDbPort =
  return $
    "docker run -d --rm --name "
      ++ devDbPortHolderContainerName
      ++ " -p "
      ++ defaultPort
      ++ ":"
      ++ defaultPort
      ++ " alpine sleep 300"
  where
    defaultPort = show defaultPostgresPort

removeDefaultDevDbPortHolder :: ShellCommandBuilder WaspProjectContext ShellCommand
removeDefaultDevDbPortHolder = return $ "docker rm -f " ++ devDbPortHolderContainerName

removeDevDbVolume :: ShellCommandBuilder WaspProjectContext ShellCommand
removeDevDbVolume = do
  context <- ask
  let db = Dev.Postgres.makeDevPostgresDbSpec context.waspProjectDir minimalStarterAppName defaultPostgresPort
  return $ "docker volume rm " ++ db.dockerVolumeName

devDbPortHolderContainerName :: String
devDbPortHolderContainerName = "wasp-e2e-tests-db-port-holder"

minimalStarterAppName :: String
minimalStarterAppName = "waspApp"
