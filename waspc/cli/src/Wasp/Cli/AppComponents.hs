module Wasp.Cli.AppComponents where

import Wasp.AppSpec (AppSpec)
import Wasp.Env (EnvVar)
import qualified Wasp.Generator.ServerGenerator.Common as Server
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import Wasp.Project.PerAppComponent (PerAppComponent (..))

devUrls :: AppSpec -> PerAppComponent String
devUrls spec =
  PerAppComponent
    { client = WebApp.getDefaultDevClientUrl spec,
      server = Server.defaultDevServerUrl
    }

devEnvVars :: AppSpec -> PerAppComponent [EnvVar]
devEnvVars spec =
  PerAppComponent
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
