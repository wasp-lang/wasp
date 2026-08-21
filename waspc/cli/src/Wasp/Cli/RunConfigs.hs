module Wasp.Cli.RunConfigs
  ( makeRunConfigs,
    showRunConfigUrls,
  )
where

import Wasp.AppComponentUrl (AppComponentUrl (..))
import qualified Wasp.AppComponentUrl as AppComponentUrl
import Wasp.Generator.ServerGenerator.RunConfig (ServerRunConfig, makeServerRunConfig)
import qualified Wasp.Generator.ServerGenerator.RunConfig as ServerRunConfig
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig, makeWebAppRunConfig)
import qualified Wasp.Generator.WebAppGenerator.RunConfig as WebAppRunConfig

makeRunConfigs :: (AppComponentUrl, AppComponentUrl) -> (WebAppRunConfig, ServerRunConfig)
makeRunConfigs (clientUrl, serverUrl) = (clientRunConfig, serverRunConfig)
  where
    clientRunConfig = makeWebAppRunConfig clientUrl (AppComponentUrl.url serverUrl)
    serverRunConfig = makeServerRunConfig serverUrl (AppComponentUrl.url clientUrl)

showRunConfigUrls :: (WebAppRunConfig, ServerRunConfig) -> String
showRunConfigUrls (clientRunConfig, serverRunConfig) =
  unlines
    [ " ℹ Client: "
        ++ ensureTrailingSlash (AppComponentUrl.url $ WebAppRunConfig.url clientRunConfig),
      " ℹ Server: "
        ++ ensureTrailingSlash (AppComponentUrl.url $ ServerRunConfig.url serverRunConfig)
    ]
  where
    -- The server and client URLs have different expectations for trailing
    -- slashes, so for display consistency we just ensure they both have it.
    ensureTrailingSlash url = if last url == '/' then url else url ++ "/"
