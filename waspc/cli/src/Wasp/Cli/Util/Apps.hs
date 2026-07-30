module Wasp.Cli.Util.Apps where

import Network.Socket (PortNumber)
import Wasp.AppSpec (AppSpec)
import Wasp.Env (EnvVar)
import qualified Wasp.Generator.ServerGenerator.Common as Server
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import Wasp.Project.Apps (Apps (..))

getDevUrlMakers :: AppSpec -> Apps (PortNumber -> String)
getDevUrlMakers spec =
  Apps
    { client = WebApp.getDevClientUrl spec,
      server = Server.makeDevServerUrl
    }

getWaspEnvVars :: AppSpec -> Apps PortNumber -> Apps [EnvVar]
getWaspEnvVars spec ports =
  Apps
    { client = WebApp.getDevClientEnvVars,
      server = Server.getDevServerEnvVars
    }
    <*> pure locations
  where
    locations = liftA2 (,) ports urls
    urls = getDevUrlMakers spec <*> ports

-- | The ports the apps run on in development. Processes that never bind a port
-- (like the test runner) also use these to build the app's URLs.
defaultAppPorts :: Apps PortNumber
defaultAppPorts =
  Apps
    { client = 3000,
      server = 3001
    }
