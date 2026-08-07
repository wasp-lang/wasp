module Wasp.Cli.Services where

import Wasp.AppSpec (AppSpec)
import Wasp.Env (EnvVar)
import qualified Wasp.Generator.ServerGenerator.Common as Server
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import Wasp.Project.PerService (PerService (..))

devUrls :: AppSpec -> PerService String
devUrls spec =
  PerService
    { client = WebApp.getDefaultDevClientUrl spec,
      server = Server.defaultDevServerUrl
    }

devEnvVars :: AppSpec -> PerService [EnvVar]
devEnvVars spec =
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
    urls = devUrls spec
