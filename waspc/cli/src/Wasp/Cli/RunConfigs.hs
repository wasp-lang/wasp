module Wasp.Cli.RunConfigs
  ( makeDefaultDevRunConfigs,
    makeRunConfigs,
    showRunConfigUrls,
  )
where

import Wasp.AppComponentUrl (AppComponentUrl (..))
import qualified Wasp.AppComponentUrl as AppComponentUrl
import Wasp.AppSpec (AppSpec)
import Wasp.Cli.AppComponentUrls (makeDefaultUrls)
import Wasp.Generator.ServerGenerator.RunConfig (ServerRunConfig, makeServerRunConfig)
import qualified Wasp.Generator.ServerGenerator.RunConfig as ServerRunConfig
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig, makeWebAppRunConfig)
import qualified Wasp.Generator.WebAppGenerator.RunConfig as WebAppRunConfig

makeDefaultDevRunConfigs :: AppSpec -> (WebAppRunConfig, ServerRunConfig)
makeDefaultDevRunConfigs appSpec = makeRunConfigs $ makeDefaultUrls appSpec

makeRunConfigs :: (AppComponentUrl, AppComponentUrl) -> (WebAppRunConfig, ServerRunConfig)
makeRunConfigs (clientUrl, serverUrl) = (clientRunConfig, serverRunConfig)
  where
    clientRunConfig = makeWebAppRunConfig clientUrl (AppComponentUrl.url serverUrl)
    serverRunConfig = makeServerRunConfig serverUrl (AppComponentUrl.url clientUrl)

showRunConfigUrls :: (WebAppRunConfig, ServerRunConfig) -> String
showRunConfigUrls (clientRunConfig, serverRunConfig) =
  unlines
    [ " ℹ Client: " ++ showUrl (WebAppRunConfig.url clientRunConfig),
      " ℹ Server: " ++ showUrl (ServerRunConfig.url serverRunConfig)
    ]
  where
    showUrl appComponentUrl =
      ensureTrailingSlash (AppComponentUrl.url appComponentUrl)
        ++ case appComponentUrl.customUrl of
          Just _ -> " (local: " ++ ensureTrailingSlash (AppComponentUrl.localUrl appComponentUrl) ++ ")"
          Nothing -> ""

    -- The server and client URLs have different expectations for trailing
    -- slashes, so for display consistency we just ensure they both have it.
    ensureTrailingSlash url = if last url == '/' then url else url ++ "/"
