module Wasp.Cli.Util.PerService where

import Network.Socket (PortNumber)
import Wasp.AppSpec (AppSpec)
import Wasp.Env (EnvVar)
import qualified Wasp.Generator.ServerGenerator.Common as Server
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import Wasp.Project.PerService (PerService (..))

getDevUrlMakers :: AppSpec -> PerService (PortNumber -> String)
getDevUrlMakers spec =
  PerService
    { client = WebApp.getDevClientUrl spec,
      server = Server.makeDevServerUrl
    }

getWaspEnvVars :: AppSpec -> PerService PortNumber -> PerService [EnvVar]
getWaspEnvVars spec ports =
  PerService
    { client = WebApp.getDevClientEnvVars,
      server = Server.getDevServerEnvVars
    }
    <*> pure locations
  where
    locations = liftA2 (,) ports urls
    urls = getDevUrlMakers spec <*> ports

-- | The ports the apps run on in development. Processes that never bind a port
-- (like the test runner) also use these to build the app's URLs.
defaultAppPorts :: PerService PortNumber
defaultAppPorts =
  PerService
    { client = 3000,
      server = 3001
    }
