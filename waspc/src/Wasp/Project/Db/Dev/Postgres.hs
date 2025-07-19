-- | This module captures how Wasp runs a PostgreSQL dev database.
module Wasp.Project.Db.Dev.Postgres
  ( defaultDevUser,
    makeDevDbName,
    defaultDevPass,
    defaultDevPort,
    makeDevConnectionUrl,
  )
where

import StrongPath (Abs, Dir, Path')
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

makeDevConnectionUrl :: Path' Abs (Dir WaspProjectDir) -> String -> String
makeDevConnectionUrl waspProjectDir appName =
  makeConnectionUrl defaultDevUser defaultDevPass defaultDevPort $ makeDevDbName waspProjectDir appName
