module Wasp.Generator.Server.RunConfig
  ( ServerRunConfig (..),
    makeDefault,
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

-- | For now, the server always runs on the default port. Making it
-- configurable comes later.
makeDefault :: ServerRunConfig
makeDefault =
  ServerRunConfig
    { port = fromIntegral Common.defaultServerPort,
      clientUrl = Nothing
    }

url :: ServerRunConfig -> String
url config = "http://localhost:" ++ show config.port

-- | The env vars Wasp sets for the server process. The client URL is omitted
-- while the server isn't wired to a client.
devEnvVars :: ServerRunConfig -> [EnvVar]
devEnvVars config =
  [(Common.clientUrlEnvVarName, clientUrl) | clientUrl <- maybeToList config.clientUrl]
    ++ [(Common.serverUrlEnvVarName, url config)]
