module Wasp.Db.Postgres
  ( makeConnectionUrl,
    postgresMaxDbNameLength,
    defaultPostgresPort,
    defaultPostgresDockerImageSpec,
  )
where

import Network.Socket (PortNumber)
import Text.Printf (printf)
import Wasp.Util.Docker
  ( DockerImageName,
    DockerVolumeMountPath,
  )

makeConnectionUrl :: String -> String -> PortNumber -> String -> String
makeConnectionUrl user pass port dbName =
  printf "postgresql://%s:%s@localhost:%s/%s" user pass (show port) dbName

-- As specified by PostgreSQL documentation.
postgresMaxDbNameLength :: Int
postgresMaxDbNameLength = 63

defaultPostgresPort :: PortNumber
defaultPostgresPort = 5432

-- | We pin the Postgres Docker image to avoid issues when a new major version of Postgres
-- is released. We aim to occasionally update this version in Wasp releases.
-- If you bump the Postgres version here, also update the default database images in
-- `waspc/data/packages/deploy/src/providers/fly/index.ts` and
-- `waspc/data/packages/deploy/src/providers/railway/index.ts`, and check that
-- their default volume mount paths are still correct.
defaultPostgresDockerImageSpec :: (DockerImageName, DockerVolumeMountPath)
defaultPostgresDockerImageSpec = ("postgres:18", dockerVolumeMountPath)
  where
    -- Path inside the Postgres Docker container where the database files are stored.
    dockerVolumeMountPath :: DockerVolumeMountPath
    dockerVolumeMountPath = "/var/lib/postgresql"
