module Wasp.Cli.RunConfigs
  ( makeDefaultDevRunConfigs,
    makeRunConfigs,
    showRunConfigUrls,
  )
where

import Wasp.AppComponentUrl (AppComponentUrl (..))
import qualified Wasp.AppComponentUrl as AppComponentUrl
import Wasp.AppSpec (AppSpec)
import Wasp.AppSpec.App.Deployment (DeploymentMode (..))
import Wasp.AppSpec.Valid (getDeploymentMode)
import Wasp.Cli.AppComponentUrls (makeDefaultUrls)
import Wasp.Generator.ServerGenerator.RunConfig (ServerRunConfig, makeServerRunConfig)
import qualified Wasp.Generator.ServerGenerator.RunConfig as ServerRunConfig
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig, makeWebAppRunConfig)
import qualified Wasp.Generator.WebAppGenerator.RunConfig as WebAppRunConfig

makeDefaultDevRunConfigs :: AppSpec -> (WebAppRunConfig, ServerRunConfig)
makeDefaultDevRunConfigs appSpec = makeRunConfigs (getDeploymentMode appSpec) (makeDefaultUrls appSpec)

makeRunConfigs :: DeploymentMode -> (AppComponentUrl, AppComponentUrl) -> (WebAppRunConfig, ServerRunConfig)
makeRunConfigs deploymentMode (clientUrl, serverUrl) = (clientRunConfig, serverRunConfig)
  where
    clientRunConfig = makeWebAppRunConfig deploymentMode clientUrl serverUrl
    serverRunConfig = makeServerRunConfig deploymentMode serverUrl clientUrl

-- | Shows the URLs of the running app components in development.
showRunConfigUrls :: DeploymentMode -> (WebAppRunConfig, ServerRunConfig) -> String
showRunConfigUrls deploymentMode (clientRunConfig, serverRunConfig) =
  unlines $ case deploymentMode of
    Single ->
      [ " ℹ App:    " ++ showUrl (WebAppRunConfig.url clientRunConfig),
        "   For debugging purposes, you can reach the server directly here:",
        " ℹ Server: " ++ showUrl (ServerRunConfig.url serverRunConfig)
      ]
    Split ->
      [ " ℹ Client: " ++ showUrl (WebAppRunConfig.url clientRunConfig),
        " ℹ Server: " ++ showUrl (ServerRunConfig.url serverRunConfig)
      ]

-- The server and client URLs have different expectations for trailing
-- slashes, so for display consistency we just ensure they both have it.
showUrl :: AppComponentUrl -> String
showUrl = ensureTrailingSlash . AppComponentUrl.url
  where
    ensureTrailingSlash url = if last url == '/' then url else url ++ "/"
