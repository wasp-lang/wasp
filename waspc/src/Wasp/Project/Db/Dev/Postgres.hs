-- | This module captures how Wasp runs a PostgreSQL dev database.
module Wasp.Project.Db.Dev.Postgres
  ( makeDevPostgresDb,
    runDevPostgresDb,
    DevDbInfo (..),
    discoverDevDb,
    defaultDevPort,
    waspDevDbDockerVolumePrefix,
  )
where

import Network.Socket (PortNumber)
import StrongPath (Abs, Dir, Path')
import System.Process (callCommand)
import Text.Printf (printf)
import Wasp.Db.Postgres (makeConnectionUrl, postgresMaxDbNameLength)
import Wasp.Project.Common (WaspProjectDir, makeAppUniqueId)
import Wasp.Util.Docker (DockerImageName, DockerVolumeMountPath, getDockerContainerHostPort)

data DevDbInfo = DevDbInfo
  { connectionUrl :: String,
    dockerVolumeName :: String,
    dockerContainerName :: String,
    dbName :: String,
    user :: String,
    password :: String,
    port :: PortNumber
  }

makeDevPostgresDb :: Path' Abs (Dir WaspProjectDir) -> String -> PortNumber -> DevDbInfo
makeDevPostgresDb waspProjectDir appName port =
  DevDbInfo
    { connectionUrl = makeConnectionUrl defaultDevUser defaultDevPass port dbName,
      dockerVolumeName = makeWaspDevDbDockerVolumeName waspProjectDir appName,
      dockerContainerName = makeWaspDevDbDockerContainerName waspProjectDir appName,
      dbName,
      user = defaultDevUser,
      password = defaultDevPass,
      port
    }
  where
    dbName = makeDevDbName waspProjectDir appName

runDevPostgresDb :: DevDbInfo -> DockerImageName -> DockerVolumeMountPath -> IO ()
runDevPostgresDb devDbInfo dbDockerImage dbDockerVolumeMountPath =
  callCommand runDbCommand
  where
    -- NOTE: POSTGRES_PASSWORD, POSTGRES_USER, POSTGRES_DB below are really used by the docker image
    --   only when initializing the database -> if the volume was created previously, they will be ignored.
    --   This is how the postgres Docker image works.
    runDbCommand =
      unwords
        [ "docker run",
          printf "--name %s" devDbInfo.dockerContainerName,
          "--rm",
          printf "--publish %s:5432" (show devDbInfo.port),
          printf "-v %s:%s" devDbInfo.dockerVolumeName dbDockerVolumeMountPath,
          printf "--env POSTGRES_PASSWORD=%s" devDbInfo.password,
          printf "--env POSTGRES_USER=%s" devDbInfo.user,
          printf "--env POSTGRES_DB=%s" devDbInfo.dbName,
          dbDockerImage
        ]

-- | Returns info about this Wasp project's dev db if it is up,
-- 'Nothing' otherwise.
discoverDevDb :: Path' Abs (Dir WaspProjectDir) -> String -> IO (Maybe DevDbInfo)
discoverDevDb waspProjectDir appName = do
  devDbPort <- getDockerContainerHostPort devDbContainerName 5432
  return $ makeDevPostgresDb waspProjectDir appName <$> devDbPort
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

defaultDevPort :: PortNumber
defaultDevPort = 5432 -- 5432 is default port for PostgreSQL db.

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
