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
      postgresTestCase
        "migration-reuses-running-database"
        [ waspCliDbStartInBackground (Just 15432),
          waitUntilDevDbReportsItIsReady,
          waspCliDbMigrateDev "first_migration",
          assertDevDbRunning,
          assertCommandOutputContains
            (return waspCliDbStartFails)
            "PostgreSQL already running",
          stopDevDbAndWait,
          assertDevDbRemoved,
          removeReportedDevDbVolume
        ],
      postgresTestCase
        "startup-uses-another-port-when-default-is-occupied"
        [ occupyDefaultDevDbPort,
          waspCliDbStartInBackground Nothing,
          waitUntilDevDbReportsItIsReady,
          waspCliDbMigrateDev "first_migration",
          assertDevDbRunning,
          removeDefaultDevDbPortHolder,
          stopDevDbAndWait,
          assertDevDbRemoved,
          removeReportedDevDbVolume
        ],
      postgresTestCase
        "migration-starts-and-removes-database"
        [ return "$WASP_CLI_CMD db migrate-dev --name first_migration --db-port 15433",
          assertDevDbRemoved
        ],
      postgresTestCase
        "reset-starts-and-removes-database"
        [ return "$WASP_CLI_CMD db reset --force --db-port 15434",
          assertDevDbRemoved
        ]
    ]
  where
    waspCliDbStartFails :: ShellCommand
    waspCliDbStartFails = "! $WASP_CLI_CMD db start"

postgresTestCase :: String -> [ShellCommandBuilder WaspProjectContext ShellCommand] -> TestCase
postgresTestCase name commands =
  TestCase name
    $ skipIfDockerDisabled
    $ sequence
      [ createTestWaspProject minimalStarterTemplate,
        inTestWaspProjectDir $ [setWaspDbToPSQL, installDbCleanup] ++ commands
      ]

-- | `wasp db start` runs the database in the foreground, so we background it.
-- We capture its output to test whether it correctly reports its readiness and volume name.
waspCliDbStartInBackground :: Maybe Int -> ShellCommandBuilder WaspProjectContext ShellCommand
waspCliDbStartInBackground port =
  return $
    "rm -f " ++ devDbOutputFile ++ " && { $WASP_CLI_CMD db start" ++ portOption ++ " > " ++ devDbOutputFile ++ " 2>&1 & echo $! > db-start.pid ; }"
  where
    portOption = maybe "" ((" --db-port " ++) . show) port

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
occupyDefaultDevDbPort = do
  containerName <- devDbPortHolderContainerName
  return $
    "docker run -d --rm --name "
      ++ containerName
      ++ " -p "
      ++ defaultPort
      ++ ":"
      ++ defaultPort
      ++ " alpine sleep 300"
  where
    defaultPort = show defaultPostgresPort

removeDefaultDevDbPortHolder :: ShellCommandBuilder WaspProjectContext ShellCommand
removeDefaultDevDbPortHolder = ("docker rm -f " ++) <$> devDbPortHolderContainerName

removeReportedDevDbVolume :: ShellCommandBuilder WaspProjectContext ShellCommand
removeReportedDevDbVolume =
  return $ "docker volume rm \"" ++ reportedDevDbVolumeName ++ "\""

-- | Shell substitution that extracts the volume name `wasp db start` reported
-- because it's the same place users learn it from.
reportedDevDbVolumeName :: String
reportedDevDbVolumeName =
  "$(grep -o -m 1 '" ++ Dev.Postgres.waspDevDbDockerVolumePrefix ++ "[a-zA-Z0-9_-]*' " ++ devDbOutputFile ++ ")"

devDbPortHolderContainerName :: ShellCommandBuilder WaspProjectContext String
devDbPortHolderContainerName = do
  db <- testDevDb
  return $ db.dockerContainerName ++ "-port-holder"

testDevDb :: ShellCommandBuilder WaspProjectContext Dev.Postgres.DevDbSpec
testDevDb = do
  context <- ask
  return $ Dev.Postgres.makeDevPostgresDbSpec context.waspProjectDir "waspApp" defaultPostgresPort

assertDevDbRunning :: ShellCommandBuilder WaspProjectContext ShellCommand
assertDevDbRunning =
  return $ "test -n \"$(docker ps -q --filter \"volume=" ++ reportedDevDbVolumeName ++ "\")\""

assertDevDbRemoved :: ShellCommandBuilder WaspProjectContext ShellCommand
assertDevDbRemoved = do
  db <- testDevDb
  return $ "test -z \"$(docker ps -aq --filter \"volume=" ++ db.dockerVolumeName ++ "\")\""

installDbCleanup :: ShellCommandBuilder WaspProjectContext ShellCommand
installDbCleanup = do
  db <- testDevDb
  portHolder <- devDbPortHolderContainerName
  return $
    "trap 'if [ -f db-start.pid ]; then kill \"$(cat db-start.pid)\" 2>/dev/null || true; wait \"$(cat db-start.pid)\" 2>/dev/null || true; fi; docker rm -f "
      ++ db.dockerContainerName
      ++ " "
      ++ portHolder
      ++ " >/dev/null 2>&1 || true; docker volume rm "
      ++ db.dockerVolumeName
      ++ " >/dev/null 2>&1 || true' EXIT"
