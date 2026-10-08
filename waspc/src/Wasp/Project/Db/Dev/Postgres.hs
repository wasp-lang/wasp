-- | This module captures how Wasp runs a PostgreSQL dev database.
module Wasp.Project.Db.Dev.Postgres
  ( makeDevPostgresDbSpec,
    createDevPostgresContainer,
    waitForDevDbReady,
    DevDbSpec (..),
    getDevConnectionUrl,
    discoverProjectsRunningDevDb,
    waspDevDbDockerVolumePrefix,
  )
where

import Control.Monad (unless)
import Network.Socket (PortNumber)
import StrongPath (Abs, Dir, Path')
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (create_group), proc, readCreateProcessWithExitCode, readProcessWithExitCode)
import Text.Printf (printf)
import Wasp.Db.Postgres (defaultPostgresPort, makeConnectionUrl, postgresMaxDbNameLength)
import Wasp.Project.Common (WaspProjectDir, makeAppUniqueId)
import Wasp.Util.Docker (DockerImageName, DockerVolumeMountPath, discoverHostPortForDockerContainersInternalPort)
import qualified Wasp.Util.IO.Retry as Retry
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

createDevPostgresContainer :: DevDbSpec -> DockerImageName -> DockerVolumeMountPath -> IO String
createDevPostgresContainer db image mountPath = do
  -- create_group keeps Docker outside the terminal's Ctrl+C group so it can return the container ID needed for cleanup.
  (status, output, errors) <- readCreateProcessWithExitCode ((proc "docker" args) {create_group = True}) ""
  case (status, words output) of
    (ExitSuccess, [containerId]) -> return containerId
    _ -> ioError $ userError $ printf "Could not create PostgreSQL container. %s" errors
  where
    -- NOTE: POSTGRES_PASSWORD, POSTGRES_USER, POSTGRES_DB below are really used by the docker image
    --   only when initializing the database -> if the volume was created previously, they will be ignored.
    --   This is how the postgres Docker image works.
    args =
      concat
        [ ["create", "--rm"],
          ["--name", db.dockerContainerName],
          ["--publish", printf "%s:%s" (show db.port) (show defaultPostgresPort)],
          ["-v", printf "%s:%s" db.dockerVolumeName mountPath],
          ["--env", printf "POSTGRES_PASSWORD=%s" db.password],
          ["--env", printf "POSTGRES_USER=%s" db.user],
          ["--env", printf "POSTGRES_DB=%s" db.dbName],
          [image]
        ]

waitForDevDbReady :: DevDbSpec -> IO ()
waitForDevDbReady devDbSpec = do
  ready <- Retry.retryUntil (Retry.constPause 1000000) 59 $ isDevDbReady devDbSpec
  unless ready $ do
    (_, output, errors) <- readProcessWithExitCode "docker" ["logs", "--tail", "30", devDbSpec.dockerContainerName] ""
    ioError $ userError $ printf "PostgreSQL did not become ready. Check the logs below, then try again.\n%s%s" output errors

isDevDbReady :: DevDbSpec -> IO Bool
isDevDbReady devDbSpec = do
  databaseReady <- isDevDbConnectionReady devDbSpec
  if databaseReady
    then Socket.checkIfPortIsAcceptingConnections $ Socket.makeLocalHostSocketAddress devDbSpec.port
    else return False

isDevDbConnectionReady :: DevDbSpec -> IO Bool
isDevDbConnectionReady devDbSpec = do
  (exitCode, output, _) <- readProcessWithExitCode "docker" args ""
  return $ exitCode == ExitSuccess && output == "1\n"
  where
    args =
      concat
        [ ["exec", "--env", printf "PGPASSWORD=%s" devDbSpec.password],
          [devDbSpec.dockerContainerName, "psql"],
          ["-h", "127.0.0.1"],
          ["-U", devDbSpec.user],
          ["-d", devDbSpec.dbName],
          ["-t", "-A", "-c", "SELECT 1"]
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
