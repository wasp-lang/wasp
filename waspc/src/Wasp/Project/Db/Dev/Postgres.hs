-- | This module captures how Wasp runs a PostgreSQL dev database.
module Wasp.Project.Db.Dev.Postgres
  ( makeDevPostgresDbSpec,
    runDevPostgresDb,
    DevDbSpec (..),
    getDevConnectionUrl,
    discoverProjectsRunningDevDb,
    defaultPostgresPort,
    waspDevDbDockerVolumePrefix,
  )
where

import Network.Socket (PortNumber)
import StrongPath (Abs, Dir, Path')
import System.Process (callCommand)
import Text.Printf (printf)
import Wasp.Db.Postgres (makeConnectionUrl, postgresMaxDbNameLength)
import Wasp.Project.Common (WaspProjectDir, makeAppUniqueId)
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

runDevPostgresDb :: DevDbSpec -> DockerImageName -> DockerVolumeMountPath -> IO ()
runDevPostgresDb devDbSpec dbDockerImage dbDockerVolumeMountPath =
  callCommand runDbCommand
  where
    -- NOTE: POSTGRES_PASSWORD, POSTGRES_USER, POSTGRES_DB below are really used by the docker image
    --   only when initializing the database -> if the volume was created previously, they will be ignored.
    --   This is how the postgres Docker image works.
    runDbCommand =
      unwords
        [ "docker run",
          printf "--name %s" devDbSpec.dockerContainerName,
          "--rm",
          printf "--publish %s:%s" (show devDbSpec.port) (show defaultPostgresPort),
          printf "-v %s:%s" devDbSpec.dockerVolumeName dbDockerVolumeMountPath,
          printf "--env POSTGRES_PASSWORD=%s" devDbSpec.password,
          printf "--env POSTGRES_USER=%s" devDbSpec.user,
          printf "--env POSTGRES_DB=%s" devDbSpec.dbName,
          dbDockerImage
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

defaultPostgresPort :: PortNumber
defaultPostgresPort = 5432

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
