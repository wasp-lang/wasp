module E2e.Resource
  ( Resource (..),
    exclusiveResource,
    devDatabasePort,
    waspDevServerPorts,
    waspStudioPort,
    prismaStudioPort,
  )
where

-- | Something that only a limited number of tests can use at the same time.
--
-- Tests run in parallel by default. When two tests would interfere with each
-- other (e.g. both bind the same host port), they declare the same resource and
-- the runner makes sure that at most 'capacity' of them run at once.
data Resource = Resource
  { name :: String,
    capacity :: Int
  }
  deriving (Eq, Ord, Show)

-- | A resource that only one test can use at a time.
exclusiveResource :: String -> Resource
exclusiveResource resourceName = Resource {name = resourceName, capacity = 1}

-- | The default PostgreSQL port that `wasp db start` binds to.
devDatabasePort :: Resource
devDatabasePort = exclusiveResource "dev-database-port"

-- | The client and server ports used by `wasp start` and `wasp build start`.
waspDevServerPorts :: Resource
waspDevServerPorts = exclusiveResource "wasp-dev-server-ports"

waspStudioPort :: Resource
waspStudioPort = exclusiveResource "wasp-studio-port"

prismaStudioPort :: Resource
prismaStudioPort = exclusiveResource "prisma-studio-port"
