module Wasp.Db.Postgres
  ( makeConnectionUrl,
    postgresMaxDbNameLength,
    defaultDockerImageForPostgres,
  )
where

import Text.Printf (printf)
import Wasp.Util.Docker (DockerImageName)

makeConnectionUrl :: String -> String -> Int -> String -> String
makeConnectionUrl user pass port dbName =
  printf "postgresql://%s:%s@localhost:%d/%s" user pass port dbName

-- As specified by PostgreSQL documentation.
postgresMaxDbNameLength :: Int
postgresMaxDbNameLength = 63

defaultDockerImageForPostgres :: DockerImageName
defaultDockerImageForPostgres = "postgres"
