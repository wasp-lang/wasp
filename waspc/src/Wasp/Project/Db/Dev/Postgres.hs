-- | This module captures how Wasp runs a PostgreSQL dev database.
module Wasp.Project.Db.Dev.Postgres
  ( defaultDevUser,
    makeDevDbName,
    defaultDevPass,
    makeDevPort,
    makeDevConnectionUrl,
  )
where

import Data.Char (digitToInt)
import StrongPath (Abs, Dir, Path', fromAbsDir)
import qualified Wasp.Db.Postgres as Db.Postgres
import Wasp.Project.Common (WaspProjectDir, makeAppUniqueId)
import qualified Wasp.Util as U

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
  take Db.Postgres.postgresMaxDbNameLength $ makeAppUniqueId waspProjectDir appName

-- | Returns a host port for the dev database that is unique for this Wasp project.
-- It depends on the project path and app name, so two Wasp projects on the same
-- machine get different ports and can run `wasp start db` concurrently, and no
-- project collides with a locally-installed PostgreSQL listening on 5432.
--
-- Because this port is a pure function of (waspProjectDir, appName), both
-- `wasp start db` and the connection URL baked into the generated server `.env`
-- agree on the port without any cross-process hand-off.
--
-- NOTE: Like `makeAppUniqueId`, the hash is computed from the raw project path,
-- so invoking wasp from a symlinked path versus the canonical path would yield
-- a different port. This matches the existing behavior for dev DB name and
-- Docker volume name.
makeDevPort :: Path' Abs (Dir WaspProjectDir) -> String -> Int
makeDevPort waspProjectDir appName =
  devPortRangeStart + (hashAsInt `mod` devPortRangeSize)
  where
    -- Take only 7 hex chars (28 bits) from the checksum so the result always
    -- fits into a non-negative Int, even on a hypothetical 32-bit build.
    hashAsInt = foldl (\acc c -> acc * 16 + digitToInt c) 0 (take 7 hashHex)
    hashHex = U.hexToString (U.checksumFromString (fromAbsDir waspProjectDir <> appName))
    -- 15432–20431: above 1024 (unprivileged), below 32768 (safe from ephemeral
    -- ranges on all OSs), recognizable as "5432 + 10000" for Wasp dev DBs.
    devPortRangeStart = 15432
    devPortRangeSize = 5000

makeDevConnectionUrl :: Path' Abs (Dir WaspProjectDir) -> String -> String
makeDevConnectionUrl waspProjectDir appName =
  Db.Postgres.makeConnectionUrl
    defaultDevUser
    defaultDevPass
    (makeDevPort waspProjectDir appName)
    (makeDevDbName waspProjectDir appName)
