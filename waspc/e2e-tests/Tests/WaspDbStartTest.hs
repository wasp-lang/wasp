module Tests.WaspDbStartTest (waspDbStartTest) where

import Control.Monad.Reader (ask)
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
        (return [waspCliDbStartFails]),
      TestCase
        "succeed-sqlite-project"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ waspCliDbStart
                ]
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
      -- NOTE: Tasty runs test cases in parallel, so the PostgreSQL scenarios (which
      -- compete for the same host ports) are all in a single sequential test case.
      TestCase
        "succeed-postgresql-project"
        ( skipIfDockerDisabled . sequence $
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir $
                concat
                  [ [setWaspDbToPSQL, installDbCleanup],
                    -- Migration reuses a separately started database and leaves it running.
                    [ waspCliDbStartInBackground,
                      waitUntilDevDbReportsItIsReady,
                      waspCliDbMigrateDev "first_migration",
                      assertDevDbRunning,
                      assertCommandOutputContains
                        (return waspCliDbStartFails)
                        "PostgreSQL already running",
                      stopDevDbAndWait,
                      assertDevDbRemoved
                    ],
                    -- Startup finds another port when the default is occupied.
                    [ occupyDefaultDevDbPort,
                      waspCliDbStartInBackground,
                      waitUntilDevDbReportsItIsReady,
                      waspCliDbMigrateDev "no_new_migration",
                      assertDevDbRunning,
                      removeDefaultDevDbPortHolder,
                      stopDevDbAndWait,
                      assertDevDbRemoved
                    ],
                    -- Migration starts and removes its own database.
                    [ waspCliDbMigrateDev "automatic_start",
                      assertDevDbRemoved
                    ],
                    -- Reset starts and removes its own database.
                    [ return "$WASP_CLI_CMD db reset --force",
                      assertDevDbRemoved
                    ],
                    -- The reported data volume remains available for explicit removal.
                    [removeReportedDevDbVolume]
                  ]
            ]
        )
    ]
  where
    waspCliDbStartFails :: ShellCommand
    waspCliDbStartFails = "! $WASP_CLI_CMD db start"

-- | `wasp db start` runs the database in the foreground, so we background it.
-- We capture its output to test whether it correctly reports its readiness and volume name.
waspCliDbStartInBackground :: ShellCommandBuilder WaspProjectContext ShellCommand
waspCliDbStartInBackground =
  return $
    "rm -f " ++ devDbOutputFile ++ " && { $WASP_CLI_CMD db start > " ++ devDbOutputFile ++ " 2>&1 & echo $! > db-start.pid ; }"

-- Stop PostgreSQL directly so this test does not depend on terminal signal forwarding.
stopDevDbAndWait :: ShellCommandBuilder WaspProjectContext ShellCommand
stopDevDbAndWait =
  return $
    "{ docker kill --signal INT \"$(docker ps -q --filter \"volume="
      ++ reportedDevDbVolumeName
      ++ "\")\" && { wait \"$(cat db-start.pid)\" || true; } && rm db-start.pid; }"

-- Wasp prints the volume name after its database readiness check succeeds.
waitUntilDevDbReportsItIsReady :: ShellCommandBuilder WaspProjectContext ShellCommand
waitUntilDevDbReportsItIsReady =
  return $
    "{ retries=180; until grep -q '"
      ++ Dev.Postgres.waspDevDbDockerVolumePrefix
      ++ "' "
      ++ devDbOutputFile
      ++ "; do retries=$((retries - 1)); [ \"$retries\" -gt 0 ] || exit 1; sleep 1; done ; }"

devDbOutputFile :: FilePath
devDbOutputFile = "db-start-output.log"

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

removeReportedDevDbVolume :: ShellCommandBuilder WaspProjectContext ShellCommand
removeReportedDevDbVolume =
  return $ "docker volume rm \"" ++ reportedDevDbVolumeName ++ "\""

-- | Shell substitution that extracts the volume name `wasp db start` reported
-- because it's the same place users learn it from.
reportedDevDbVolumeName :: String
reportedDevDbVolumeName =
  "$(grep -o -m 1 '" ++ Dev.Postgres.waspDevDbDockerVolumePrefix ++ "[a-zA-Z0-9_-]*' " ++ devDbOutputFile ++ ")"

devDbPortHolderContainerName :: String
devDbPortHolderContainerName = "wasp-e2e-tests-db-port-holder"

assertDevDbRunning :: ShellCommandBuilder WaspProjectContext ShellCommand
assertDevDbRunning =
  return $ "test -n \"$(docker ps -q --filter \"volume=" ++ reportedDevDbVolumeName ++ "\")\""

assertDevDbRemoved :: ShellCommandBuilder WaspProjectContext ShellCommand
assertDevDbRemoved =
  return $ "test -z \"$(docker ps -aq --filter \"volume=" ++ reportedDevDbVolumeName ++ "\")\""

installDbCleanup :: ShellCommandBuilder WaspProjectContext ShellCommand
installDbCleanup = do
  context <- ask
  let db = Dev.Postgres.makeDevPostgresDbSpec context.waspProjectDir "waspApp" defaultPostgresPort
  return $
    "trap 'if [ -f db-start.pid ]; then kill \"$(cat db-start.pid)\" 2>/dev/null || true; wait \"$(cat db-start.pid)\" 2>/dev/null || true; fi; docker rm -f "
      ++ db.dockerContainerName
      ++ " "
      ++ devDbPortHolderContainerName
      ++ " >/dev/null 2>&1 || true' EXIT"
