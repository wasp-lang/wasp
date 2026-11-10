module Tests.WaspDbStartTest (waspDbStartTest) where

import Control.Monad (void)
import Data.Char (isSpace)
import qualified Data.Text as T
import E2E
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
        $ wasp ["db", "start"],
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
              -- Does a Wasp command find the database and connect to it after
              -- `wasp db start` says it's ready?
              waspDbMigrateDev "first_migration"
              -- Does the second `wasp db start` detect and report an already
              -- running dev database?
              waspFails ["db", "start"] >>= assertOutputContains "already running on port"

              -- Does SIGINT stop the database and delete the container, even
              -- while a client is still connected to it?
              volumeName <- getReportedVolumeName devDb
              containerName <- getDevDbContainerName volumeName
              holdDevDbSessionOpen volumeName
              stopDevDbWithSignal SIGINT devDb containerName volumeName

              -- Does `wasp db start` work when an earlier one left its
              -- container behind, e.g. when it was stopped while creating it?
              -- Does `wasp db start` find a new port when the default one is taken?
              leaveStoppedContainerBehind containerName
              occupyDefaultPostgresPort
              devDbOnOtherPort <- startDevDb
              -- Does a Wasp command find and connect to the database even when
              -- it's running on a non-default port?
              waspDbMigrateDev "no_new_migration"
              freeDefaultPostgresPort

              -- Does SIGTERM (e.g. from a process manager) also stop the
              -- database and delete the container?
              stopDevDbWithSignal SIGTERM devDbOnOtherPort containerName volumeName

              -- Can the user remove the volume reported by `wasp db start`?
              docker ["volume", "rm", volumeName]
        ]
    ]

-- | `wasp db start` runs the database in the foreground until it's stopped.
startDevDb :: TestM RunningProcess
startDevDb = do
  devDb <- startInBackground $ waspCli ["db", "start"]
  waitForOutput devDb "listening on IPv4 address" 180
  return devDb

-- | Stops `wasp db start` with the signal, and checks that Wasp exits and that
-- the database container is gone.
--
-- We send the signal only to Wasp, and not to its whole process group like
-- Ctrl+C in a terminal would, so that it's Wasp's job to stop the database.
stopDevDbWithSignal :: Signal -> RunningProcess -> String -> String -> TestM ()
stopDevDbWithSignal sig devDb containerName volumeName = do
  getWaspPid >>= signal sig
  waitForExit devDb 60
  waitUntil ("the dev database container is gone after " ++ show sig) 30 $
    null . dockerOutputLines <$> docker ["ps", "-a", "-q", "--filter", "volume=" ++ volumeName]
  where
    -- The process we started might not be Wasp itself (e.g. when the Wasp CLI
    -- runs through a wrapper script), so we look for the parent of the
    -- `docker run` client instead, which is Wasp.
    getWaspPid = do
      dockerRunPid <- firstLine <$> run (command "pgrep" ["-f", "docker run --name " ++ containerName ++ " "])
      read . firstLine <$> run (command "ps" ["-o", "ppid=", "-p", dockerRunPid])

    firstLine = takeWhile (/= '\n') . dropWhile isSpace . T.unpack . (.stdout)

-- | Opens a database session that stays open, like the app's server's.
holdDevDbSessionOpen :: String -> TestM ()
holdDevDbSessionOpen volumeName = do
  containerId <- firstOutputLine <$> docker ["ps", "-q", "--filter", "volume=" ++ volumeName]
  docker ["exec", "-d", containerId, "sh", "-c", psqlCommand "select pg_sleep(600)"]
  waitUntil "the database session opens" 30 $
    (/= "0") . firstOutputLine
      <$> docker
        [ "exec",
          containerId,
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
    _ -> failTest "`wasp db start` didn't report its volume name."

getDevDbContainerName :: String -> TestM String
getDevDbContainerName volumeName =
  firstOutputLine <$> docker ["ps", "--format", "{{.Names}}", "--filter", "volume=" ++ volumeName]

-- | Creates a stopped container with the name of the dev db container, like
-- the one that `docker run` leaves behind when it's stopped while creating it.
leaveStoppedContainerBehind :: String -> TestM ()
leaveStoppedContainerBehind containerName = void $ docker ["create", "--name", containerName, "alpine"]

occupyDefaultPostgresPort :: TestM ()
occupyDefaultPostgresPort = do
  let port = show defaultPostgresPort
  registerCleanupStep freeDefaultPostgresPort
  void $ docker ["run", "-d", "--rm", "--name", portHolderContainerName, "-p", port ++ ":" ++ port, "alpine", "sleep", "300"]

freeDefaultPostgresPort :: TestM ()
freeDefaultPostgresPort = void . runAllowingFailure $ command "docker" ["rm", "-f", portHolderContainerName]

portHolderContainerName :: String
portHolderContainerName = "wasp-e2e-tests-db-port-holder"

docker :: [String] -> TestM ProcessResult
docker = run . command "docker"

dockerOutputLines :: ProcessResult -> [String]
dockerOutputLines = filter (not . null) . lines . T.unpack . (.stdout)

firstOutputLine :: ProcessResult -> String
firstOutputLine result = case dockerOutputLines result of
  firstLine : _ -> firstLine
  [] -> ""
