module Wasp.Cli.Services
  ( defaultDevPorts,
    devUrls,
    devEnvVars,
    showServiceUrls,
  )
where

import Network.Socket (PortNumber)
import Wasp.AppSpec (AppSpec)
import Wasp.Env (EnvVar)
import qualified Wasp.Generator.ServerGenerator.Common as Server
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import Wasp.Project.PerService (PerService (..))

-- | The ports the apps run on in development when the user doesn't choose any.
-- Processes that never bind a port (like the test runner) also use these as
-- placeholders.
defaultDevPorts :: PerService PortNumber
defaultDevPorts =
  PerService
    { client = 3000,
      server = 3001
    }

devUrls :: AppSpec -> PerService PortNumber -> PerService String
devUrls spec ports = getDevServiceUrl <*> pure spec <*> ports

devEnvVars :: PerService PortNumber -> PerService String -> PerService [EnvVar]
devEnvVars ports urls = getDevServiceEnvVars <*> ports <*> pure urls

getDevServiceUrl :: PerService (AppSpec -> PortNumber -> String)
getDevServiceUrl =
  PerService
    { client = WebApp.getDevClientUrl,
      server = const Server.getDevServerUrl
    }

getDevServiceEnvVars :: PerService (PortNumber -> PerService String -> [EnvVar])
getDevServiceEnvVars =
  PerService
    { client = WebApp.getDevClientEnvVars,
      server = Server.getDevServerEnvVars
    }

showServiceUrls :: PerService String -> String
showServiceUrls urls =
  unlines
    [ " ℹ Client: " ++ ensureTrailingSlash urls.client,
      " ℹ Server: " ++ ensureTrailingSlash urls.server
    ]
  where
    -- The server and client URLs have different expectations for trailing
    -- slashes, so for display consistency we just ensure they both have it.
    ensureTrailingSlash url = if last url == '/' then url else url ++ "/"
