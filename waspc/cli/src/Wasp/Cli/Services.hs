module Wasp.Cli.Services where

import Network.Socket (PortNumber)
import Wasp.AppSpec (AppSpec)
import Wasp.Env (EnvVar)
import qualified Wasp.Generator.ServerGenerator.Common as Server
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import Wasp.Project.PerService (PerService (..))

-- | The ports the apps run on in development. Processes that never bind a port
-- (like the test runner) also use these to build the app's URLs.
devPorts :: PerService PortNumber
devPorts =
  PerService
    { client = 3000,
      server = 3001
    }

devUrls :: AppSpec -> PerService PortNumber -> PerService String
devUrls spec ports =
  PerService
    { client = WebApp.getDevClientUrl spec,
      server = Server.getDevServerUrl
    }
    <*> ports

-- | Each app gets its own port and the URLs of both apps, since they need to
-- know where to reach each other.
devEnvVars :: PerService PortNumber -> PerService String -> PerService [EnvVar]
devEnvVars ports urls =
  PerService
    { client = WebApp.getDevClientEnvVars,
      server = Server.getDevServerEnvVars
    }
    <*> ports
    <*> pure urls
