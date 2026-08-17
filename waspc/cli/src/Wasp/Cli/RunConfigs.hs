module Wasp.Cli.RunConfigs where

import Network.Socket (PortNumber)
import Wasp.AppComponentUrl (AppComponentUrl (..))
import qualified Wasp.AppComponentUrl as AppComponentUrl
import Wasp.AppSpec (AppSpec)
import Wasp.Generator.ServerGenerator.RunConfig (ServerRunConfig, makeServerRunConfig)
import qualified Wasp.Generator.WebAppGenerator.Common as WebAppG
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig, makeWebAppRunConfig)

makeDevDefaultRunConfigs :: AppSpec -> (WebAppRunConfig, ServerRunConfig)
makeDevDefaultRunConfigs appSpec =
  uncurry makeRunConfigs $ makeDefaultUrls appSpec

makeDefaultUrls :: AppSpec -> (AppComponentUrl, AppComponentUrl)
makeDefaultUrls appSpec = (clientUrl, serverUrl)
  where
    clientUrl = makeDefaultDevClientUrl appSpec
    serverUrl = defaultDevServerUrl

makeRunConfigs :: AppComponentUrl -> AppComponentUrl -> (WebAppRunConfig, ServerRunConfig)
makeRunConfigs clientUrl serverUrl = (clientRunConfig, serverRunConfig)
  where
    clientRunConfig = makeWebAppRunConfig clientUrl (AppComponentUrl.url serverUrl)
    serverRunConfig = makeServerRunConfig serverUrl (AppComponentUrl.url clientUrl)

showAppComponentUrls :: (AppComponentUrl, AppComponentUrl) -> String
showAppComponentUrls (clientUrl, serverUrl) =
  unlines
    [ " ℹ Client: " ++ ensureTrailingSlash (AppComponentUrl.url clientUrl),
      " ℹ Server: " ++ ensureTrailingSlash (AppComponentUrl.url serverUrl)
    ]
  where
    -- The server and client URLs have different expectations for trailing
    -- slashes, so for display consistency we just ensure they both have it.
    ensureTrailingSlash url = if last url == '/' then url else url ++ "/"

makeDefaultDevClientUrl :: AppSpec -> AppComponentUrl
makeDefaultDevClientUrl spec =
  Local {port = defaultDevClientPort, path = Just $ WebAppG.getBaseDir spec}

defaultDevServerUrl :: AppComponentUrl
defaultDevServerUrl =
  Local {port = defaultDevServerPort, path = Nothing}

defaultDevClientPort :: PortNumber
defaultDevClientPort = 3000

defaultDevServerPort :: PortNumber
defaultDevServerPort = 3001
