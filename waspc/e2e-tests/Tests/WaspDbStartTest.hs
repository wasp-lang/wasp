module Tests.WaspDbStartTest (waspDbStartTest) where

import Control.Monad (void)
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
    [ test "fail-outside-project" $ waspFails ["db", "start"],
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
          test "succeed-postgresql-project" $ do
            skipIfDockerDisabled
            withNewWaspProject minimalStarterTemplate $ do
              setDbToPostgres

              -- Does `wasp db start` work?
              devDb <- startDevDb
              devDbDockerNames <- getDevDbDockerNames devDb
              registerCleanupStep $ removeDevDbContainerAndVolume devDbDockerNames
              -- Does a Wasp command find the database and connect to it after
              -- `wasp db start` says it's ready?
              waspDbMigrateDev "first_migration"
              -- Does the second `wasp db start` detect and report an already
              -- running dev database?
              waspFails ["db", "start"] >>= assertOutputContains "already running on port"

              -- Does SIGINT stop the database and delete the container, even
              -- while a client is still connected to it?
              holdDevDbSessionOpen devDbDockerNames
              stopDevDbWithSignal SIGINT devDb devDbDockerNames

              -- Does `wasp db start` work when an earlier one left its
              -- container behind, e.g. when it was stopped while creating it?
              -- Does `wasp db start` find a new port when the default one is taken?
              leaveStoppedContainerBehind devDbDockerNames
              portHolderContainerName <- occupyDefaultPostgresPort
              devDbOnOtherPort <- startDevDb
              -- Does it use the same volume, so the data is still there?
              getReportedVolumeName devDbOnOtherPort
                >>= assertEqual "volume reported by the second `wasp db start`" devDbDockerNames.volumeName
              -- Does a Wasp command find and connect to the database even when
              -- it's running on a non-default port?
              waspDbMigrateDev "no_new_migration"
              freeDefaultPostgresPort portHolderContainerName

              -- Does SIGTERM (e.g. from a process manager) also stop the
              -- database and delete the container?
              stopDevDbWithSignal SIGTERM devDbOnOtherPort devDbDockerNames

              -- Does `wasp db start` fail when the database stops before
              -- accepting connections?
              waspFails ["db", "start", "--db-image", alpineImage]
                >>= assertOutputContains "The database stopped before accepting connections"
              -- Does `wasp db start` fail when it can't check the connection,
              -- e.g. when the image isn't a PostgreSQL image?
              waspFails ["db", "start", "--db-image", nginxImage]
                >>= assertOutputContains "Unable to check the database connection"

              -- Can the user remove the volume reported by `wasp db start`?
              docker ["volume", "rm", devDbDockerNames.volumeName]
        ]
    ]

-- | `wasp db start` runs the database in the foreground until it's stopped.
startDevDb :: TestM RunningProcess
startDevDb = do
  devDb <- startInBackground $ waspCli ["db", "start"]
  waitForOutput devDb "PostgreSQL dev database is ready" (3 * 60)
  return devDb

-- | The names of the dev database's Docker container and volume, which Wasp
-- derives from the project.
data DevDbDockerNames = DevDbDockerNames
  { containerName :: String,
    volumeName :: String
  }

-- | Reads the names while the dev database runs.
getDevDbDockerNames :: RunningProcess -> TestM DevDbDockerNames
getDevDbDockerNames devDb = do
  volumeName <- getReportedVolumeName devDb
  containerName <- firstOutputLine =<< docker ["ps", "--format", "{{.Names}}", "--filter", "volume=" ++ volumeName]
  return DevDbDockerNames {containerName, volumeName}

-- | For when the test fails before Wasp removes the container or before the
-- test removes the volume. Otherwise they're already gone, which is fine.
removeDevDbContainerAndVolume :: DevDbDockerNames -> TestM ()
removeDevDbContainerAndVolume devDbDockerNames = do
  void $ dockerAllowingFailure ["rm", "-f", devDbDockerNames.containerName]
  void $ dockerAllowingFailure ["volume", "rm", "-f", devDbDockerNames.volumeName]

-- | Stops `wasp db start` with the signal, and checks that Wasp exits and that
-- the database container is gone.
--
-- We send the signal only to Wasp, and not to its whole process group like
-- Ctrl+C in a terminal would, so that it's Wasp's job to stop the database.
stopDevDbWithSignal :: Signal -> RunningProcess -> DevDbDockerNames -> TestM ()
stopDevDbWithSignal sig devDb devDbDockerNames = do
  getWaspPid >>= signalPid sig
  waitUntilStopped devDb []
  waitUntil ("the dev database container is gone after " ++ show sig) 30 $
    null . stdoutLines <$> docker ["ps", "-a", "-q", "--filter", "volume=" ++ devDbDockerNames.volumeName]
  where
    -- The process we started might not be Wasp itself (e.g. when the Wasp CLI
    -- runs through a wrapper script), so we look for the parent of the
    -- `docker run` client instead, which is Wasp.
    getWaspPid = do
      dockerRunPid <- firstOutputLine =<< run (command "pgrep" ["-f", "docker run --name " ++ devDbDockerNames.containerName ++ " "])
      psResult <- run (command "ps" ["-o", "ppid=", "-p", dockerRunPid])
      waspPid <- firstOutputLine psResult
      maybe (failWithResult psResult "printed an invalid parent PID") return (readMaybe waspPid)

-- | Opens a database session that stays open, like the app's server's.
holdDevDbSessionOpen :: DevDbDockerNames -> TestM ()
holdDevDbSessionOpen devDbDockerNames = do
  docker ["exec", "-d", devDbDockerNames.containerName, "sh", "-c", psqlCommand "select pg_sleep(600)"]
  waitUntil "the database session opens" 30 $
    fmap (/= "0") . firstOutputLine
      =<< docker
        [ "exec",
          devDbDockerNames.containerName,
          "sh",
          "-c",
          psqlCommand "select count(*) from pg_stat_activity where pid <> pg_backend_pid() and datname = current_database()"
        ]
  where
    -- Runs in the container's shell, which knows the database's credentials.
    psqlCommand sql = "psql -U \"$POSTGRES_USER\" -d \"$POSTGRES_DB\" -tAc \"" ++ sql ++ "\""

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

-- | Runs a container that publishes the default PostgreSQL port, and returns
-- its name, which is unique, so that the tests of other checkouts can't
-- remove it.
occupyDefaultPostgresPort :: TestM String
occupyDefaultPostgresPort = do
  containerName <- makeUniqueName "wasp-e2e-tests-db-port-holder"
  registerCleanupStep $ freeDefaultPostgresPort containerName
  void $ docker ["run", "-d", "--rm", "--name", containerName, "-p", port ++ ":" ++ port, alpineImage, "sleep", show portHoldSeconds]
  return containerName
  where
    port = show defaultPostgresPort
    -- Longer than the steps that need the port taken, which start the dev
    -- database (see 'startDevDb'), so the container doesn't stop before them.
    portHoldSeconds = 5 * 60 :: Int

-- | Doesn't fail when the container is already gone, since it's also the
-- cleanup, which runs after the test already freed the port.
freeDefaultPostgresPort :: String -> TestM ()
freeDefaultPostgresPort portHolderContainerName = void $ dockerAllowingFailure ["rm", "-f", portHolderContainerName]

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

docker :: [String] -> TestM ProcessResult
docker = run . command "docker"

dockerAllowingFailure :: [String] -> TestM ProcessResult
dockerAllowingFailure = runAllowingFailure . command "docker"

-- | The non-empty lines of stdout, without surrounding whitespace.
stdoutLines :: ProcessResult -> [String]
stdoutLines = filter (not . null) . map (T.unpack . T.strip) . T.lines . (.stdout)

-- | Fails the test if the command printed nothing.
firstOutputLine :: ProcessResult -> TestM String
firstOutputLine result = case stdoutLines result of
  firstLine : _ -> return firstLine
  [] -> failWithResult result "printed nothing to stdout"
