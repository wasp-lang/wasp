module Wasp.Generator.Server
  ( ServerRunConfig (..),
    defaultPort,
    make,
    url,
    devEnvVars,
  )
where

import Data.Maybe (maybeToList)
import Network.Socket (PortNumber)
import Wasp.Env (EnvVar)
import qualified Wasp.Generator.ServerGenerator.Common as Common

-- | Configuration for a run of the app's server: where it runs and what it
-- knows about its surroundings.
data ServerRunConfig = ServerRunConfig
  { port :: PortNumber,
    -- | URL of the client this server serves. 'Nothing' until the server is
    -- wired to a client (see 'Wasp.Cli.AppComponents.makeDevRunConfigs').
    clientUrl :: Maybe String
  }
  deriving (Show, Eq)

-- | The port the server runs on in development when the user doesn't choose
-- one. Processes that never bind a port (like the test runner) also use it as
-- a placeholder.
defaultPort :: PortNumber
defaultPort = 3001

make :: PortNumber -> ServerRunConfig
make port =
  ServerRunConfig
    { port = port,
      clientUrl = Nothing
    }

url :: ServerRunConfig -> String
url config = "http://localhost:" ++ show config.port

-- | The env vars Wasp sets for the server process. The client URL is omitted
-- while the server isn't wired to a client.
devEnvVars :: ServerRunConfig -> [EnvVar]
devEnvVars config =
  [ (Common.serverPortEnvVarName, show config.port),
    (Common.serverUrlEnvVarName, url config)
  ]
    ++ [(Common.clientUrlEnvVarName, clientUrl) | clientUrl <- maybeToList config.clientUrl]
