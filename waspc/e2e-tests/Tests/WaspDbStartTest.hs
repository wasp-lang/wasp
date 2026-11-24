module Tests.WaspDbStartTest (waspDbStartTest) where

import Control.Monad (unless, void)
import Data.Char (isSpace)
import qualified Data.Text as T
import E2E
import Text.Read (readMaybe)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Db.Postgres (defaultPostgresPort)
import qualified Wasp.Project.Db.Dev.Postgres as Dev.Postgres

waspDbStartTest :: E2ETest
waspDbStartTest =
  testGroup
    "wasp-db-start"
    [ test "fail-outside-project" $ waspFailsOutsideProject ["db", "start"],
      test "succeed-sqlite-project"
        $ withNewWaspProject minimalStarterTemplate
        $ wasp ["db", "start"]
          >>= assertOutputContains "you are using SQLite which doesn't need to be started",
      -- These tests expect the default PostgreSQL port to be free or take it.
      sequentialTestGroup
        "postgresql"
        [ test "fail-postgresql-project-when-dev-db-not-running" $
            withNewWaspProject minimalStarterTemplate $ do
              setDbToPostgres
              waspFails ["db", "migrate-dev"] >>= assertOutputContains "The database needs to be running",
          dockerTest "succeed-postgresql-project" $
            withNewWaspProject minimalStarterTemplate $ do
              setDbToPostgres
              -- Before the first start, so that a start that fails halfway
              -- is cleaned up too.
              devDbDockerNames <- getDevDbDockerNames
              registerCleanupStep $ removeDevDbContainerAndVolume devDbDockerNames

              devDb <- step "`wasp db start` starts the database" $ do
                devDb <- startDevDb
                -- Also proves that we derived the names right, since the
                -- cleanup can't tell a wrong name from a container or volume
                -- that's already gone.
                getReportedVolumeName devDb
                  >>= assertEqual "volume reported by `wasp db start`" devDbDockerNames.volumeName
                return devDb
              step "a Wasp command finds the database and connects to it once `wasp db start` says it's ready" $
                waspDbMigrateDev "first_migration"
              step "a second `wasp db start` detects and reports the running database" $
                devDbStartFails [] >>= assertOutputContains "already running on port"
              step "SIGINT stops the database and deletes the container, even while a client is connected" $ do
                holdDevDbSessionOpen devDbDockerNames
                stopDevDbWithSignal SIGINT devDb devDbDockerNames

              -- One start checks both, since each start takes a while.
              devDbOnOtherPort <-
                step "`wasp db start` works when an earlier one left its container behind, and finds a new port when the default one is taken" $ do
                  -- E.g. when the earlier one was stopped while creating it.
                  leaveStoppedContainerBehind devDbDockerNames
                  occupyDefaultPostgresPort
                  startDevDb
              step "the new database uses the same volume, so the data is still there" $
                getReportedVolumeName devDbOnOtherPort
                  >>= assertEqual "volume reported by the second `wasp db start`" devDbDockerNames.volumeName
              step "a Wasp command finds the database and connects to it on a non-default port" $
                waspDbMigrateDev "no_new_migration"
              step "SIGTERM (e.g. from a process manager) also stops the database and deletes the container" $
                stopDevDbWithSignal SIGTERM devDbOnOtherPort devDbDockerNames

              step "`wasp db start` fails when the database stops before accepting connections" $
                devDbStartFails ["--db-image", alpineImage]
                  >>= assertOutputContains "The database stopped before accepting connections"
              step "`wasp db start` fails when it can't check the connection, e.g. when the image isn't a PostgreSQL image" $
                devDbStartFails ["--db-image", nginxImage]
                  >>= assertOutputContains "Unable to check the database connection"

              -- `docker volume rm` fails while a container uses the volume, so
              -- this also checks that no dev database container is left.
              step "the user can remove the volume reported by `wasp db start`" $
                docker ["volume", "rm", devDbDockerNames.volumeName]
        ]
    ]

-- | `wasp db start` runs the database in the foreground until it's stopped.
startDevDb :: TestM RunningProcess
startDevDb = do
  devDb <- startInBackground $ waspCli ["db", "start"]
  waitForOutput devDb "PostgreSQL dev database is ready" devDbReadyTimeoutSeconds
  return devDb

-- | Pulling the image, creating the volume, and PostgreSQL's first start.
devDbReadyTimeoutSeconds :: Int
devDbReadyTimeoutSeconds = 3 * 60

-- | Runs @wasp db start@ with extra arguments, expecting it to fail before the
-- database is up.
devDbStartFails :: [String] -> TestM ProcessResult
devDbStartFails arguments =
  runExpectingFailure (waspCli (["db", "start"] ++ arguments)) {timeoutSeconds = failFastTimeoutSeconds}

-- | The names of the dev database's Docker container and volume, which Wasp
-- derives from the project.
data DevDbDockerNames = DevDbDockerNames
  { containerName :: String,
    volumeName :: String
  }

-- | Derives the names like Wasp does, so that the test can register their
-- cleanup before anything creates them.
getDevDbDockerNames :: TestM DevDbDockerNames
getDevDbDockerNames = do
  (projectDir, appName) <- getProjectDirAndAppName
  -- The names don't depend on the port.
  let devDbSpec = Dev.Postgres.makeDevPostgresDbSpec projectDir appName defaultPostgresPort
  return DevDbDockerNames {containerName = devDbSpec.dockerContainerName, volumeName = devDbSpec.dockerVolumeName}

-- | For when the test fails before Wasp removes the container or before the
-- test removes the volume. Otherwise they're already gone, which is fine.
removeDevDbContainerAndVolume :: DevDbDockerNames -> TestM ()
removeDevDbContainerAndVolume devDbDockerNames = do
  dockerCleanup ["rm", "--force", devDbDockerNames.containerName]
  dockerCleanup ["volume", "rm", "--force", devDbDockerNames.volumeName]

-- | Stops `wasp db start` with the signal, and checks that Wasp exits and that
-- the database container is gone.
--
-- We send the signal only to Wasp, and not to its whole process group like
-- Ctrl+C in a terminal would, so that it's Wasp's job to stop the database.
stopDevDbWithSignal :: Signal -> RunningProcess -> DevDbDockerNames -> TestM ()
stopDevDbWithSignal sig devDb devDbDockerNames = do
  getWaspPid >>= signalPid sig
  waitUntilExited devDb
  waitUntil ("the dev database container is gone after " ++ show sig) settleTimeoutSeconds $
    null . stdoutLines <$> dockerWithinWait ["ps", "--all", "--quiet", "--filter", "volume=" ++ devDbDockerNames.volumeName]
  where
    -- The process we started might not be Wasp itself (e.g. when the Wasp CLI
    -- runs through a wrapper script), so we look for the parent of the
    -- `docker run` client instead, which is Wasp.
    getWaspPid = do
      -- Matches `docker run ... --name <container> ...`, which only Wasp runs.
      pgrepResult <- runAllowingFailure $ command "pgrep" ["-f", "docker run .*--name " ++ devDbDockerNames.containerName ++ " "]
      unless (pgrepResult.exitCode == ExitSuccess) $
        failWithResult pgrepResult "found no `docker run` process of the dev database"
      dockerRunPid <- firstStdoutLine pgrepResult
      psResult <- run (command "ps" ["-o", "ppid=", "-p", dockerRunPid])
      waspPid <- firstStdoutLine psResult
      maybe (failWithResult psResult "printed an invalid parent PID") return (readMaybe waspPid)

-- | Opens a database session that stays open, like the app's server's.
holdDevDbSessionOpen :: DevDbDockerNames -> TestM ()
holdDevDbSessionOpen devDbDockerNames = do
  docker ["exec", "--detach", devDbDockerNames.containerName, "sh", "-c", psqlCommand $ "select pg_sleep(" ++ show sessionHoldSeconds ++ ")"]
  waitUntil "the database session opens" settleTimeoutSeconds $
    fmap (/= "0") . firstStdoutLine
      =<< dockerWithinWait
        [ "exec",
          devDbDockerNames.containerName,
          "sh",
          "-c",
          psqlCommand "select count(*) from pg_stat_activity where pid <> pg_backend_pid() and datname = current_database()"
        ]
  where
    -- Runs in the container's shell, which knows the database's credentials.
    psqlCommand sql = "psql -U \"$POSTGRES_USER\" -d \"$POSTGRES_DB\" -tAc \"" ++ sql ++ "\""
    -- Longer than the test needs the session. Stopping the database ends it.
    sessionHoldSeconds = 10 * 60 :: Int

-- | For the conditions of 'waitUntil', which only checks its deadline between
-- checks: a hung `docker` command mustn't outlast the wait.
dockerWithinWait :: [String] -> TestM ProcessResult
dockerWithinWait arguments = run (dockerCommand arguments) {timeoutSeconds = settleTimeoutSeconds}

-- | The volume name `wasp db start` reported, since that's where users learn it from.
getReportedVolumeName :: RunningProcess -> TestM String
getReportedVolumeName devDb = do
  output <- getOutput devDb
  let prefix = T.pack Dev.Postgres.waspDevDbDockerVolumePrefix
  case T.breakOn prefix output of
    (_, rest) | not (T.null rest) -> return . T.unpack $ T.takeWhile (not . isSpace) rest
    _ -> failWithOutput devDb "didn't report its volume name"

-- | Creates a stopped container with the name of the dev db container, like
-- the one that `docker run` leaves behind when it's stopped while creating it.
leaveStoppedContainerBehind :: DevDbDockerNames -> TestM ()
leaveStoppedContainerBehind devDbDockerNames = void $ docker ["create", "--name", devDbDockerNames.containerName, alpineImage]

-- | Runs a container that publishes the default PostgreSQL port until the test
-- ends. Its name is unique, so that the tests of other checkouts can't remove
-- it.
--
-- If something else already listens on the port (e.g. a local PostgreSQL, or
-- this test of another checkout), the port is already taken, as we want.
occupyDefaultPostgresPort :: TestM ()
occupyDefaultPostgresPort = do
  isPortTaken <- isListening defaultPostgresPortNumber
  if isPortTaken
    then logLine $ "\n==> Port " ++ port ++ " is already taken, so we don't start a container to take it."
    else do
      containerName <- makeUniqueName "wasp-e2e-tests-db-port-holder"
      registerCleanupStep $ dockerCleanup ["rm", "--force", containerName]
      void $ docker ["run", "--detach", "--rm", "--name", containerName, "--publish", port ++ ":" ++ port, alpineImage, "sleep", "infinity"]
  where
    defaultPostgresPortNumber = fromIntegral defaultPostgresPort
    port = show defaultPostgresPortNumber

-- | For containers that only need to exist, or to run a command. As the dev
-- database image, it exits right away, since it has no server to run. Any
-- version works: we pin it so that a new release can't change what the test
-- does.
alpineImage :: String
alpineImage = "alpine:3.23"

-- | For a container that keeps running but isn't PostgreSQL. Pinned like
-- 'alpineImage'.
nginxImage :: String
nginxImage = "nginx:1.31-alpine"
