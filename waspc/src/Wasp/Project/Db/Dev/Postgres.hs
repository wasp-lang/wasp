-- | This module captures how Wasp runs a PostgreSQL dev database.
module Wasp.Project.Db.Dev.Postgres
  ( defaultDevUser,
    makeDevDbName,
    defaultDevPass,
    defaultDevPort,
    devDbPortEnvVarName,
    getDevDbPort,
    makeDevConnectionUrl,
  )
where

import StrongPath (Abs, Dir, Path')
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Wasp.Db.Postgres (makeConnectionUrl, postgresMaxDbNameLength)
import Wasp.Project.Common (WaspProjectDir, makeAppUniqueId)

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

-- | Env var users can set to make Wasp run/connect to the dev database on a
-- custom host port, e.g. when 5432 is already taken by a local PostgreSQL server.
devDbPortEnvVarName :: String
devDbPortEnvVarName = "WASP_DEV_DB_PORT"

getDevDbPort :: IO (Either String Int)
getDevDbPort = do
  maybePortStr <- lookupEnv devDbPortEnvVarName
  return $ maybe (Right defaultDevPort) parseDevDbPort maybePortStr
  where
    parseDevDbPort :: String -> Either String Int
    parseDevDbPort portStr = case readMaybe portStr of
      Nothing -> Left $ devDbPortEnvVarName <> " must be a valid number, but it is set to empty"
      Just port
        | port > 0 && port <= 65535 -> Right port
        | otherwise -> Left $ devDbPortEnvVarName <> " must be a TCP port number between 1 and 65535, but it is set to: " <> portStr

makeDevConnectionUrl :: Int -> Path' Abs (Dir WaspProjectDir) -> String -> String
makeDevConnectionUrl devDbPort waspProjectDir appName =
  makeConnectionUrl defaultDevUser defaultDevPass devDbPort $ makeDevDbName waspProjectDir appName
