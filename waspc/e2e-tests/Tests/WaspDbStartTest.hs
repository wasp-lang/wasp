module Tests.WaspDbStartTest (waspDbStartTest) where

import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    WaspProjectContext,
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
        "fail-postgresql-project-when-dev-db-not-running"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ setWaspDbToPSQL,
                  assertCommandOutputContains
                    (return waspCliDbMigrateDevFails)
                    "The database needs to be running"
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
                  -- Test 1: Does `wasp db start` work?
                  waspCliDbStartInBackground,
                  -- Test 2: Does a Wasp command find the database and connect to it
                  -- after `wasp db start` says it's ready?
                  waitUntilDevDbReportsItIsReady,
                  waspCliDbMigrateDev "first_migration",
                  -- Test 3: Does the second `wasp db start` detect and report
                  -- an already running dev database?
                  assertCommandOutputContains
                    (return waspCliDbStartFails)
                    "already running on port",
                  -- Test 4:
                  --   - Does SIGINT stop the database and delete the container?
                  --     If it didn't delete the container, the next `wasp db start` would fail
                  --     with a container name conflict.
                  --   - Does `wasp db start` find a new port when the default one is taken?
                  stopWaspCliDbStartWithSigint,
                  occupyDefaultDevDbPort,
                  waspCliDbStartInBackground,
                  waitUntilDevDbReportsItIsReady,
                  -- Test 5: Does a Wasp command find and connect to the database
                  -- even when it's running on a non-default port?
                  waspCliDbMigrateDev "no_new_migration",
                  removeDefaultDevDbPortHolder,
                  stopWaspCliDbStartWithSigint,
                  -- Test 6: Can the user remove the volume reported by `wasp db start`?
                  removeReportedDevDbVolume
                ]
            ]
        )
    ]
  where
    waspCliDbStartFails :: ShellCommand
    waspCliDbStartFails = "! $WASP_CLI_CMD db start"

    waspCliDbMigrateDevFails :: ShellCommand
    waspCliDbMigrateDevFails = "! $WASP_CLI_CMD db migrate-dev"

-- | `wasp db start` runs the database in the foreground, so we background it.
-- We capture its output to test whether it correctly reports its readiness and volume name.
waspCliDbStartInBackground :: ShellCommandBuilder WaspProjectContext ShellCommand
waspCliDbStartInBackground =
  return $
    "rm -f " ++ devDbOutputFile ++ " && { $WASP_CLI_CMD db start > " ++ devDbOutputFile ++ " 2>&1 & echo $! > db-start.pid ; }"

-- | Stops the dev db the way Ctrl+C on `wasp db start` would - Postgres
-- receives SIGINT and shuts down.
--
-- We send the signal directly the container instead of `wasp db start` because
-- Wasp ignores SIGINT when it has a child process. Wasp expects the child to
-- handle the signal and exits after the child exits.
--
-- In a terminal, Ctrl+C sends SIGINT to the entire process group, which means
-- both Wasp and the container get it. Wasp ignores it, the container handles it
-- and exists, which then causes Wasp to exit.
stopWaspCliDbStartWithSigint :: ShellCommandBuilder WaspProjectContext ShellCommand
stopWaspCliDbStartWithSigint =
  return $
    "{ docker kill --signal INT \"$(docker ps -q --filter volume="
      ++ reportedDevDbVolumeName
      ++ ")\" && wait \"$(cat db-start.pid)\" || true ; }"

waitUntilDevDbReportsItIsReady :: ShellCommandBuilder WaspProjectContext ShellCommand
waitUntilDevDbReportsItIsReady =
  return $
    "{ retries=180; until grep -q 'listening on IPv4 address' "
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
    defaultPort = show Dev.Postgres.defaultPostgresPort

removeDefaultDevDbPortHolder :: ShellCommandBuilder WaspProjectContext ShellCommand
removeDefaultDevDbPortHolder = return $ "docker rm -f " ++ devDbPortHolderContainerName

removeReportedDevDbVolume :: ShellCommandBuilder WaspProjectContext ShellCommand
removeReportedDevDbVolume =
  return $ "docker volume rm \"" ++ reportedDevDbVolumeName ++ "\""

-- | Shell substitution that extracts the volume name `wasp db start` reported
-- because it's the same place users learn it from.
reportedDevDbVolumeName :: String
reportedDevDbVolumeName =
  "$(grep -o -m 1 '" ++ Dev.Postgres.waspDevDbDockerVolumePrefix ++ "[^ ]*' " ++ devDbOutputFile ++ ")"

devDbPortHolderContainerName :: String
devDbPortHolderContainerName = "wasp-e2e-tests-db-port-holder"
