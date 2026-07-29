module Wasp.Cli.Util.Apps where

import Wasp.AppSpec (AppSpec)
import qualified Wasp.Generator.ServerGenerator.Common as Server
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import Wasp.Project.Apps (Apps (..))

defaultDevUrls :: AppSpec -> Apps String
defaultDevUrls spec =
  Apps
    { client = WebApp.getDefaultDevClientUrl spec,
      server = Server.defaultDevServerUrl
    }

defaultPorts :: Apps Int
defaultPorts =
  Apps
    { client = WebApp.defaultClientPort,
      server = Server.defaultServerPort
    }
