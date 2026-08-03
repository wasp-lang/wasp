-- | This module captures how Wasp runs a PostgreSQL dev database.
module Wasp.Project.Db.Dev.Postgres
  ( startDevPostgresDb,
    DevPostgresDb (connectionUrl, dockerVolumeName),
    discoverDevConnectionUrl,
    defaultDevPort,
    waspDevDbDockerVolumePrefix,
  )
where

import StrongPath (Abs, Dir, Path')
import System.Process (callCommand)
import Text.Printf (printf)
import Wasp.Db.Postgres (makeConnectionUrl, postgresMaxDbNameLength)
import Wasp.Project.Common (WaspProjectDir, makeAppUniqueId)
import Wasp.Util.Docker (DockerImageName, DockerVolumeMountPath, getDockerContainerHostPort)

-- | Connection info of a Wasp project's dev db. There is no way to obtain it
-- without starting the database ('startDevPostgresDb') or discovering an
-- already running one ('discoverDevConnectionUrl').
data DevPostgresDb = DevPostgresDb
  { connectionUrl :: String,
    dockerVolumeName :: String
  }

-- | Runs a PostgreSQL dev database in a Docker container, unique for the Wasp
-- project with the specified path and name. The database's connection info
-- exists only once the database is provisioned, so it is passed to the given
-- callback right before the run. Blocks until the database shuts down.
startDevPostgresDb ::
  Path' Abs (Dir WaspProjectDir) ->
  String ->
  DockerImageName ->
  DockerVolumeMountPath ->
  (DevPostgresDb -> IO ()) ->
  IO ()
startDevPostgresDb waspProjectDir appName dbDockerImage dbDockerVolumeMountPath onDbProvisioned = do
  onDbProvisioned devPostgresDb
  callCommand dockerRunCommand
  where
    devPostgresDb =
      DevPostgresDb
        { connectionUrl = makeDevConnectionUrl waspProjectDir appName,
          dockerVolumeName = makeWaspDevDbDockerVolumeName waspProjectDir appName
        }

    -- NOTE: POSTGRES_PASSWORD, POSTGRES_USER, POSTGRES_DB below are really used by the docker image
    --   only when initializing the database -> if it already exists, they will be ignored.
    --   This is how the postgres Docker image works.
    dockerRunCommand =
      unwords
        [ "docker run",
          printf "--name %s" dockerContainerName,
          "--rm",
          printf "--publish %d:5432" defaultDevPort,
          printf "-v %s:%s" devPostgresDb.dockerVolumeName dbDockerVolumeMountPath,
          printf "--env POSTGRES_PASSWORD=%s" defaultDevPass,
          printf "--env POSTGRES_USER=%s" defaultDevUser,
          printf "--env POSTGRES_DB=%s" dbName,
          dbDockerImage
        ]

    dockerContainerName = makeWaspDevDbDockerContainerName waspProjectDir appName
    dbName = makeDevDbName waspProjectDir appName

-- | Returns the connection URL of this Wasp project's dev db if it is up,
-- 'Nothing' otherwise.
discoverDevConnectionUrl :: Path' Abs (Dir WaspProjectDir) -> String -> IO (Maybe String)
discoverDevConnectionUrl waspProjectDir appName = do
  devDbPort <- getDockerContainerHostPort devDbContainerName 5432
  return $ makeUrlOnPort <$> devDbPort
  where
    makeUrlOnPort port = makeConnectionUrl defaultDevUser defaultDevPass port devDbName
    devDbName = makeDevDbName waspProjectDir appName
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

defaultDevPort :: Int
defaultDevPort = 5432 -- 5432 is default port for PostgreSQL db.

makeDevConnectionUrl :: Path' Abs (Dir WaspProjectDir) -> String -> String
makeDevConnectionUrl waspProjectDir appName =
  makeConnectionUrl defaultDevUser defaultDevPass defaultDevPort $ makeDevDbName waspProjectDir appName

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
