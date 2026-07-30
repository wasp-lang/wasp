module Wasp.Cli.Util.Apps where

import Wasp.AppSpec (AppSpec)
import Wasp.Env (EnvVar)
import qualified Wasp.Generator.ServerGenerator.Common as Server
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import Wasp.Project.Apps (Apps (..))

getDevUrls :: AppSpec -> Apps String
getDevUrls spec =
  Apps
    { client = WebApp.getDefaultDevClientUrl spec,
      server = Server.defaultDevServerUrl
    }

getWaspEnvVars :: AppSpec -> Apps [EnvVar]
getWaspEnvVars spec =
  Apps
    { client =
        [ (WebApp.serverUrlEnvVarName, urls.server)
        ],
      server =
        [ (Server.clientUrlEnvVarName, urls.client),
          (Server.serverUrlEnvVarName, urls.server)
        ]
    }
  where
    urls = getDevUrls spec
