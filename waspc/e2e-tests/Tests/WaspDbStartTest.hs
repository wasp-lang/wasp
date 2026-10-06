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
        "fail-sqlite-project"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ assertCommandOutputContains (return waspCliDbStartFails) "SQLite uses a local file"
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
              inTestWaspProjectDir
                [ setWaspDbToPSQL,
                  installDbCleanup,
                  -- Test 1: Does `wasp db start` work?
                  waspCliDbStartInBackground,
                  -- Test 2: Does a Wasp command find the database and connect to it
                  -- after `wasp db start` says it's ready?
                  waitUntilDevDbReportsItIsReady,
                  waspCliDbMigrateDev "first_migration",
                  assertDevDbRunning,
                  -- Test 3: Does the second `wasp db start` detect and report
                  -- an already running dev database?
                  assertCommandOutputContains
                    (return "! timeout 30 $WASP_CLI_CMD db start")
                    "PostgreSQL already running",
                  -- Test 4:
                  --   - Does stopping PostgreSQL delete the container?
                  --     If it didn't delete the container, the next `wasp db start` would fail
                  --     with a container name conflict.
                  --   - Does `wasp db start` find a new port when the default one is taken?
                  stopDevDbAndWait,
                  occupyDefaultDevDbPort,
                  waspCliDbStartInBackground,
                  waitUntilDevDbReportsItIsReady,
                  -- Test 5: Does a Wasp command find and connect to the database
                  -- even when it's running on a non-default port?
                  waspCliDbMigrateDev "no_new_migration",
                  removeDefaultDevDbPortHolder,
                  stopDevDbAndWait,
                  assertDevDbRemoved,
                  waspCliDbMigrateDev "automatic_start",
                  assertDevDbRemoved,
                  return "$WASP_CLI_CMD db reset --force",
                  assertDevDbRemoved,
                  -- Test 6: Can the user remove the volume reported by `wasp db start`?
                  removeReportedDevDbVolume
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

waitUntilDevDbReportsItIsReady :: ShellCommandBuilder WaspProjectContext ShellCommand
waitUntilDevDbReportsItIsReady =
  return $
    "{ retries=180; until grep -q 'Data volume:' "
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
