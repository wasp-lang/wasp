module Generator.DevEnvVarsTest where

import Network.Socket (PortNumber)
import Test.Hspec
import qualified Wasp.Generator.ServerGenerator.Common as Server
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import Wasp.Project.Apps (Apps (..))

-- | Each side gets told its own port but the *other* side's URL, so the thing worth
-- checking is that the two don't get crossed.
locations :: Apps (PortNumber, String)
locations =
  Apps
    { client = (3000, clientUrl),
      server = (3001, serverUrl)
    }

clientUrl :: String
clientUrl = "http://localhost:3000/"

serverUrl :: String
serverUrl = "http://localhost:3001"

spec_getDevClientEnvVars :: Spec
spec_getDevClientEnvVars =
  it "tells the client its own port and the server's URL" $
    WebApp.getDevClientEnvVars locations
      `shouldMatchList` [ (WebApp.clientDevPortEnvVarName, "3000"),
                          (WebApp.serverUrlEnvVarName, serverUrl)
                        ]

spec_getDevServerEnvVars :: Spec
spec_getDevServerEnvVars =
  it "tells the server its own port and URL, and the client's URL" $
    Server.getDevServerEnvVars locations
      `shouldMatchList` [ (Server.serverPortEnvVarName, "3001"),
                          (Server.serverUrlEnvVarName, serverUrl),
                          (Server.clientUrlEnvVarName, clientUrl)
                        ]
