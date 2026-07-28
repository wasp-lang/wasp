-- | This module captures how Wasp runs a PostgreSQL dev database.
module Wasp.Project.Db.Dev.Postgres
  ( discoverDevConnectionUrl,
    defaultDevUser,
    makeDevDbName,
    defaultDevPass,
    defaultDevPort,
    makeDevConnectionUrl,
    makeWaspDevDbDockerContainerName,
    makeWaspDevDbDockerVolumeName,
    waspDevDbDockerVolumePrefix,

    -- * Exported for testing only
    parseDockerPortOutput,
  )
where

import Control.Exception (SomeException, try)
import Data.List.Extra (takeWhileEnd)
import StrongPath (Abs, Dir, Path')
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)
import Wasp.Db.Postgres (makeConnectionUrl, postgresMaxDbNameLength)
import Wasp.Project.Common (WaspProjectDir, makeAppUniqueId)

-- | Returns the connection URL of this Wasp project's dev db, on the host port
-- the dev db container is currently published on. Returns Nothing if the
-- container is not running or docker is not available.
discoverDevConnectionUrl :: Path' Abs (Dir WaspProjectDir) -> String -> IO (Maybe String)
discoverDevConnectionUrl waspProjectDir appName =
  fmap makeUrlForPort <$> discoverDevDbPort waspProjectDir appName
  where
    makeUrlForPort port = makeConnectionUrl defaultDevUser defaultDevPass port $ makeDevDbName waspProjectDir appName

-- | Asks Docker for the host port on which this Wasp project's dev db container
-- is currently published. Returns Nothing if the container is not running or
-- docker is not available.
discoverDevDbPort :: Path' Abs (Dir WaspProjectDir) -> String -> IO (Maybe Int)
discoverDevDbPort waspProjectDir appName = do
  result <-
    try $ readProcessWithExitCode "docker" ["port", containerName, "5432"] ""
  return $ case result of
    Right (ExitSuccess, stdout, _stderr) -> parseDockerPortOutput stdout
    Right (ExitFailure _, _stdout, _stderr) -> Nothing
    Left (_ :: SomeException) -> Nothing
  where
    containerName = makeWaspDevDbDockerContainerName waspProjectDir appName

-- | Parses the output of @docker port \<container\> \<port\>@ into the host port.
-- The output has a line per network interface, e.g. "0.0.0.0:5433\n[::]:5433\n".
parseDockerPortOutput :: String -> Maybe Int
parseDockerPortOutput output = case lines output of
  (firstLine : _) -> readMaybe $ takeWhileEnd (/= ':') firstLine
  [] -> Nothing

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
