module Wasp.Cli.RunConfigs where

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

makeDefaultDevClientUrl :: AppSpec -> AppComponentUrl
makeDefaultDevClientUrl spec =
  Local {port = 3000, path = Just $ WebAppG.getBaseDir spec}

defaultDevServerUrl :: AppComponentUrl
defaultDevServerUrl =
  Local {port = 3001, path = Nothing}
