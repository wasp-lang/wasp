-- | This module captures how Wasp runs a PostgreSQL dev database.
module Wasp.Project.Db.Dev.Postgres
  ( makeDevPostgresDbSpec,
    runDevPostgresDb,
    waitUntilDevDbAcceptsConnections,
    ConnectionCheck (..),
    DevDbSpec (..),
    getDevConnectionUrl,
    discoverProjectsRunningDevDb,
    waspDevDbDockerVolumePrefix,
  )
where

import Control.Concurrent (threadDelay)
import Network.Socket (PortNumber)
import StrongPath (Abs, Dir, Path')
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Text.Printf (printf)
import UnliftIO.Exception (onException)
import Wasp.Db.Postgres (defaultPostgresPort, makeConnectionUrl, postgresMaxDbNameLength)
import Wasp.Job (Job)
import qualified Wasp.Job as Job
import Wasp.Project.Common (WaspProjectDir, makeAppUniqueId)
import Wasp.Util (secondsToMicroSeconds)
import Wasp.Util.Docker (DockerImageName, DockerVolumeMountPath, discoverHostPortForDockerContainersInternalPort)

data DevDbSpec = DevDbSpec
  { dockerVolumeName :: String,
    dockerContainerName :: String,
    dbName :: String,
    user :: String,
    password :: String,
    port :: PortNumber
  }

makeDevPostgresDbSpec :: Path' Abs (Dir WaspProjectDir) -> String -> PortNumber -> DevDbSpec
makeDevPostgresDbSpec waspProjectDir appName port =
  DevDbSpec
    { dockerVolumeName = makeWaspDevDbDockerVolumeName waspProjectDir appName,
      dockerContainerName = makeWaspDevDbDockerContainerName waspProjectDir appName,
      dbName = makeDevDbName waspProjectDir appName,
      user = defaultDevUser,
      password = defaultDevPass,
      port
    }

getDevConnectionUrl :: DevDbSpec -> String
getDevConnectionUrl devDbSpec =
  makeConnectionUrl devDbSpec.user devDbSpec.password devDbSpec.port devDbSpec.dbName

-- | Runs the dev database in the foreground, until it's stopped.
runDevPostgresDb :: DevDbSpec -> DockerImageName -> DockerVolumeMountPath -> Job ExitCode
runDevPostgresDb devDbSpec dbDockerImage dbDockerVolumeMountPath = do
  -- If `docker run` is stopped while it's creating the container, the
  -- container is left behind without being started, so `--rm` never removes
  -- it, and its name would conflict with the one we're about to create.
  -- `docker rm` without `--force` doesn't remove running containers, so this
  -- never stops a database that is in use. Its output (e.g. that there is no
  -- such container) isn't useful to the user, so we drop it.
  _ <- Job.captureOutput $ Job.fromProc $ Job.proc "docker" ["rm", devDbSpec.dockerContainerName]

  -- We use `docker stop` to be good citizens, ensuring the database is properly
  -- stopped (with the image's defined STOPSIGNAL) when the job is interrupted.
  runDbContainer `onException` Job.captureOutput stopDbContainer
  where
    runDbContainer =
      Job.fromProc $
        Job.proc
          "docker"
          -- NOTE: POSTGRES_PASSWORD, POSTGRES_USER, POSTGRES_DB below are really used by the docker image
          --   only when initializing the database -> if the volume was created previously, they will be ignored.
          --   This is how the postgres Docker image works.
          [ "run",
            "--name",
            devDbSpec.dockerContainerName,
            "--rm",
            "--publish",
            printf "%s:%s" (show devDbSpec.port) (show postgresImageInternalPort),
            "-v",
            printf "%s:%s" devDbSpec.dockerVolumeName dbDockerVolumeMountPath,
            "--env",
            "POSTGRES_PASSWORD=" <> devDbSpec.password,
            "--env",
            "POSTGRES_USER=" <> devDbSpec.user,
            "--env",
            "POSTGRES_DB=" <> devDbSpec.dbName,
            dbDockerImage
          ]

    stopDbContainer = Job.fromProc $ Job.proc "docker" ["stop", devDbSpec.dockerContainerName]

    postgresImageInternalPort = defaultPostgresPort

data ConnectionCheck = AcceptsConnections | UnableToCheckConnection

-- | Waits until the dev database container accepts connections. Keeps waiting
-- while the container is still being created or initialized.
waitUntilDevDbAcceptsConnections :: DevDbSpec -> IO ConnectionCheck
waitUntilDevDbAcceptsConnections devDbSpec =
  runPgIsReady devDbSpec >>= \case
    ExitSuccess -> return AcceptsConnections
    ExitFailure exitCode | exitCode == commandNotFoundExitCode -> return UnableToCheckConnection
    ExitFailure _ -> do
      threadDelay (secondsToMicroSeconds 1)
      waitUntilDevDbAcceptsConnections devDbSpec
  where
    -- `docker exec` exits with this code when the container doesn't have the command.
    commandNotFoundExitCode = 127

runPgIsReady :: DevDbSpec -> IO ExitCode
runPgIsReady devDbSpec = do
  (exitCode, _, _) <- readProcessWithExitCode "docker" pgIsReadyArgs ""
  return exitCode
  where
    pgIsReadyArgs =
      concat
        [ ["exec", devDbSpec.dockerContainerName, "pg_isready", "--quiet"],
          -- We use 127.0.0.1 to avoid getting a success until the database is really up.
          ["--host", "127.0.0.1"],
          ["--username", devDbSpec.user],
          -- We use the `postgres` database because it always exists, so the check doesn't log a failed connection.
          ["--dbname", "postgres"]
        ]

-- | Returns all relevant info about this Wasp project's dev detabase if its
-- container is running, 'Nothing' otherwise.
discoverProjectsRunningDevDb :: Path' Abs (Dir WaspProjectDir) -> String -> IO (Maybe DevDbSpec)
discoverProjectsRunningDevDb waspProjectDir appName = do
  devDbPort <- discoverHostPortForDockerContainersInternalPort devDbContainerName defaultPostgresPort
  return $ makeDevPostgresDbSpec waspProjectDir appName <$> devDbPort
  where
    devDbContainerName = makeWaspDevDbDockerContainerName waspProjectDir appName

defaultDevUser :: String
defaultDevUser = "postgresWaspDevUser"

defaultDevPass :: String
defaultDevPass = "postgresWaspDevPass"

-- | Returns a db name that is unique for this Wasp project.
-- It depends on projects path and name, so if any of those change,
-- the db name will also change.
makeDevDbName :: Path' Abs (Dir WaspProjectDir) -> String -> String
makeDevDbName waspProjectDir appName =
  -- We use makeAppUniqueId to construct a db name instead of a hardcoded value like "waspDevDb"
  -- in order to avoid the situation where one Wasp app accidentally connects to a db that another
  -- Wasp app has started. This way db name is unique for the specific Wasp app, and another Wasp app
  -- can't connect to it by accident.
  take postgresMaxDbNameLength $ makeAppUniqueId waspProjectDir appName

-- | Docker volume name unique for the Wasp project with specified path and name.
makeWaspDevDbDockerVolumeName :: Path' Abs (Dir WaspProjectDir) -> String -> String
makeWaspDevDbDockerVolumeName waspProjectDir appName =
  take maxDockerVolumeNameLength $
    waspDevDbDockerVolumePrefix <> "-" <> makeAppUniqueId waspProjectDir appName

waspDevDbDockerVolumePrefix :: String
waspDevDbDockerVolumePrefix = "wasp-dev-db"

maxDockerVolumeNameLength :: Int
maxDockerVolumeNameLength = 255

-- | Docker container name unique for the Wasp project with specified path and name.
makeWaspDevDbDockerContainerName :: Path' Abs (Dir WaspProjectDir) -> String -> String
makeWaspDevDbDockerContainerName waspProjectDir appName =
  take maxDockerContainerNameLength $
    waspDevDbDockerVolumePrefix <> "-" <> makeAppUniqueId waspProjectDir appName

maxDockerContainerNameLength :: Int
maxDockerContainerNameLength = 63
