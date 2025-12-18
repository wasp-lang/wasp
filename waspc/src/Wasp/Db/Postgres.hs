module Wasp.Db.Postgres
  ( makeConnectionUrl,
    postgresMaxDbNameLength,
    defaultPostgresDockerImageSpec,
  )
where

import Text.Printf (printf)
import Wasp.Util.Docker
  ( DockerImageName,
    DockerVolumeMountPath,
  )

makeConnectionUrl :: String -> String -> Int -> String -> String
makeConnectionUrl user pass port dbName =
  printf "postgresql://%s:%s@localhost:%d/%s" user pass port dbName

-- As specified by PostgreSQL documentation.
postgresMaxDbNameLength :: Int
postgresMaxDbNameLength = 63

-- | We pin the Postgres Docker image to avoid issues when a new major version of Postgres
-- is released. We aim to occasionally update this version in Wasp releases.
-- If you bump the Postgres version here, also check if `dockerVolumeMountPath`
-- is still correct.
defaultPostgresDockerImageSpec :: (DockerImageName, DockerVolumeMountPath)
defaultPostgresDockerImageSpec = ("postgres:18", dockerVolumeMountPath)
  where
    -- Path inside the Postgres Docker container where the database files are stored.
    dockerVolumeMountPath :: DockerVolumeMountPath
    dockerVolumeMountPath = "/var/lib/postgresql"
