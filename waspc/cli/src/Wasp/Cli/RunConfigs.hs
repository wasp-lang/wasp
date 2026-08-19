module Wasp.Cli.RunConfigs
  ( makeDefaultDevRunConfigs,
    makeRunConfigs,
  )
where

import Wasp.AppComponentUrl (AppComponentUrl (..))
import qualified Wasp.AppComponentUrl as AppComponentUrl
import Wasp.AppSpec (AppSpec)
import Wasp.Cli.DefaultDevAppComponentUrls (makeDefaultUrls)
import Wasp.Generator.ServerGenerator.RunConfig (ServerRunConfig, makeServerRunConfig)
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig, makeWebAppRunConfig)

makeDefaultDevRunConfigs :: AppSpec -> (WebAppRunConfig, ServerRunConfig)
makeDefaultDevRunConfigs appSpec = makeRunConfigs $ makeDefaultUrls appSpec

makeRunConfigs :: (AppComponentUrl, AppComponentUrl) -> (WebAppRunConfig, ServerRunConfig)
makeRunConfigs (clientUrl, serverUrl) = (clientRunConfig, serverRunConfig)
  where
    clientRunConfig = makeWebAppRunConfig clientUrl (AppComponentUrl.url serverUrl)
    serverRunConfig = makeServerRunConfig serverUrl (AppComponentUrl.url clientUrl)
