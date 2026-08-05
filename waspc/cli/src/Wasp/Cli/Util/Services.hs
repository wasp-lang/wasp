module Wasp.Cli.Util.Services where

import Wasp.AppSpec (AppSpec)
import Wasp.Env (EnvVar)
import qualified Wasp.Generator.ServerGenerator.Common as Server
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import Wasp.Project.PerService (PerService (..))

getDevUrls :: AppSpec -> PerService String
getDevUrls spec =
  PerService
    { client = WebApp.getDefaultDevClientUrl spec,
      server = Server.defaultDevServerUrl
    }

getWaspEnvVars :: AppSpec -> PerService [EnvVar]
getWaspEnvVars spec =
  PerService
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
