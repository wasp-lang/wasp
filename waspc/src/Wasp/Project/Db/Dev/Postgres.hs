-- | This module captures how Wasp runs a PostgreSQL dev database.
module Wasp.Project.Db.Dev.Postgres
  ( makeDevPostgresDbSpec,
    createDevPostgresDb,
    isDevDbReady,
    waitForReadyDevDb,
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
import System.Process (CreateProcess (create_group), proc, readCreateProcessWithExitCode, readProcessWithExitCode)
import Wasp.Db.Postgres (defaultPostgresPort, makeConnectionUrl, postgresMaxDbNameLength)
import Wasp.Project.Common (WaspProjectDir, makeAppUniqueId)
import Wasp.Util.Docker (DockerImageName, DockerVolumeMountPath, discoverHostPortForDockerContainersInternalPort)
import qualified Wasp.Util.Network.Socket as Socket

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

createDevPostgresDb :: DevDbSpec -> DockerImageName -> DockerVolumeMountPath -> IO String
createDevPostgresDb db image mountPath = do
  (status, output, errors) <- readCreateProcessWithExitCode ((proc "docker" args) {create_group = True}) ""
  case (status, words output) of
    (ExitSuccess, [containerId]) -> return containerId
    _ -> ioError $ userError $ "Could not create PostgreSQL container. " <> errors
  where
    args =
      [ "create",
        "--name",
        db.dockerContainerName,
        "--publish",
        "127.0.0.1:" <> show db.port <> ":" <> show defaultPostgresPort,
        "-v",
        db.dockerVolumeName <> ":" <> mountPath,
        "--env",
        "POSTGRES_PASSWORD=" <> db.password,
        "--env",
        "POSTGRES_USER=" <> db.user,
        "--env",
        "POSTGRES_DB=" <> db.dbName,
        image
      ]

waitForReadyDevDb :: DevDbSpec -> IO ()
waitForReadyDevDb devDbSpec = waitForReady 60
  where
    waitForReady :: Int -> IO ()
    waitForReady 0 = do
      (_, output, errors) <- readProcessWithExitCode "docker" ["logs", "--tail", "30", devDbSpec.dockerContainerName] ""
      ioError $ userError $ "PostgreSQL did not become ready. Check the logs below, then try again.\n" <> output <> errors
    waitForReady attempts = do
      ready <- isDevDbReady devDbSpec
      if ready then return () else threadDelay 1000000 >> waitForReady (attempts - 1)

isDevDbReady :: DevDbSpec -> IO Bool
isDevDbReady devDbSpec = do
  (readinessExitCode, _, _) <-
    readProcessWithExitCode
      "docker"
      [ "exec",
        devDbSpec.dockerContainerName,
        "pg_isready",
        "-q",
        "-h",
        "127.0.0.1",
        "-U",
        devDbSpec.user,
        "-d",
        devDbSpec.dbName
      ]
      ""
  if readinessExitCode /= ExitSuccess
    then return False
    else do
      databaseReady <- isDevDbConnectionReady devDbSpec
      if databaseReady
        then Socket.checkIfPortIsAcceptingConnections $ Socket.makeLocalHostSocketAddress devDbSpec.port
        else return False

isDevDbConnectionReady :: DevDbSpec -> IO Bool
isDevDbConnectionReady devDbSpec = do
  (exitCode, output, _) <-
    readProcessWithExitCode
      "docker"
      [ "exec",
        "--env",
        "PGPASSWORD=" <> devDbSpec.password,
        devDbSpec.dockerContainerName,
        "psql",
        "-h",
        "127.0.0.1",
        "-U",
        devDbSpec.user,
        "-d",
        devDbSpec.dbName,
        "-t",
        "-A",
        "-c",
        "SELECT 1"
      ]
      ""
  return $ exitCode == ExitSuccess && output == "1\n"

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
