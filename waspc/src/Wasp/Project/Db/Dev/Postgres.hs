-- | This module captures how Wasp runs a PostgreSQL dev database.
module Wasp.Project.Db.Dev.Postgres
  ( makeDevPostgresDb,
    runDevPostgresDb,
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

data DevPostgresDb = DevPostgresDb
  { connectionUrl :: String,
    dockerVolumeName :: String,
    runDbCommand :: String
  }

makeDevPostgresDb ::
  Path' Abs (Dir WaspProjectDir) ->
  String ->
  DockerImageName ->
  DockerVolumeMountPath ->
  DevPostgresDb
makeDevPostgresDb waspProjectDir appName dbDockerImage dbDockerVolumeMountPath =
  DevPostgresDb
    { connectionUrl = makeDevConnectionUrl waspProjectDir appName defaultDevPort,
      dockerVolumeName = volumeName,
      -- NOTE: POSTGRES_PASSWORD, POSTGRES_USER, POSTGRES_DB below are really used by the docker image
      --   only when initializing the database -> if it already exists, they will be ignored.
      --   This is how the postgres Docker image works.
      runDbCommand =
        unwords
          [ "docker run",
            printf "--name %s" dockerContainerName,
            "--rm",
            printf "--publish %d:5432" defaultDevPort,
            printf "-v %s:%s" volumeName dbDockerVolumeMountPath,
            printf "--env POSTGRES_PASSWORD=%s" dbPass,
            printf "--env POSTGRES_USER=%s" defaultDevUser,
            printf "--env POSTGRES_DB=%s" dbName,
            dbDockerImage
          ]
    }
  where
    volumeName = makeWaspDevDbDockerVolumeName waspProjectDir appName
    dockerContainerName = makeWaspDevDbDockerContainerName waspProjectDir appName
    dbName = makeDevDbName waspProjectDir appName
    -- A unique password per project is necessary to prevent Wasp apps from creating databases 
    -- in each other's database servers.
    dbPass = makeDevDbPass waspProjectDir appName

runDevPostgresDb :: DevPostgresDb -> IO ()
runDevPostgresDb devPostgresDb = callCommand devPostgresDb.runDbCommand

-- | Returns the connection URL of this Wasp project's dev db if it is up,
-- 'Nothing' otherwise.
discoverDevConnectionUrl :: Path' Abs (Dir WaspProjectDir) -> String -> IO (Maybe String)
discoverDevConnectionUrl waspProjectDir appName = do
  devDbPort <- getDockerContainerHostPort devDbContainerName 5432
  return $ makeDevConnectionUrl waspProjectDir appName <$> devDbPort
  where
    devDbContainerName = makeWaspDevDbDockerContainerName waspProjectDir appName

defaultDevUser :: String
defaultDevUser = "postgresWaspDevUser"

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

makeDevDbPass :: Path' Abs (Dir WaspProjectDir) -> String -> String
makeDevDbPass = makeAppUniqueId

defaultDevPort :: Int
defaultDevPort = 5432 -- 5432 is default port for PostgreSQL db.

makeDevConnectionUrl :: Path' Abs (Dir WaspProjectDir) -> String -> Int -> String
makeDevConnectionUrl waspProjectDir appName port =
  makeConnectionUrl defaultDevUser devDbPass port devDbName
  where
    devDbName = makeDevDbName waspProjectDir appName
    devDbPass = makeDevDbPass waspProjectDir appName

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
