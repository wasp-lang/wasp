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
import Wasp.Project.BuildType (BuildType (..))

makeDefaultDevRunConfigs :: AppSpec -> (WebAppRunConfig, ServerRunConfig)
makeDefaultDevRunConfigs appSpec = makeRunConfigs (getDeploymentMode appSpec) (makeDefaultUrls appSpec)

makeRunConfigs :: DeploymentMode -> (AppComponentUrl, AppComponentUrl) -> (WebAppRunConfig, ServerRunConfig)
makeRunConfigs deploymentMode (clientUrl, serverUrl) = (clientRunConfig, serverRunConfig)
  where
    clientRunConfig = makeWebAppRunConfig deploymentMode clientUrl serverUrl
    serverRunConfig = makeServerRunConfig deploymentMode serverUrl clientUrl

-- | Shows the URLs the user reaches the running app on.
showRunConfigUrls :: BuildType -> DeploymentMode -> (WebAppRunConfig, ServerRunConfig) -> String
showRunConfigUrls buildType deploymentMode (clientRunConfig, serverRunConfig) =
  unlines $ case (deploymentMode, buildType) of
    -- The client dev server proxies to the server, so the app lives on the client's URL and
    -- the server's own URL is only useful for debugging.
    (Single, Development) ->
      [ appUrlLine,
        "   For debugging purposes, you can reach the server directly here:",
        serverUrlLine
      ]
    -- The server serves the built client, so there is nothing but the app's URL.
    (Single, Production) -> [appUrlLine]
    (Split, _) -> [clientUrlLine, serverUrlLine]
  where
    appUrlLine = " ℹ App:    " ++ showUrl (WebAppRunConfig.url clientRunConfig)
    clientUrlLine = " ℹ Client: " ++ showUrl (WebAppRunConfig.url clientRunConfig)
    serverUrlLine = " ℹ Server: " ++ showUrl (ServerRunConfig.url serverRunConfig)

-- The server and client URLs have different expectations for trailing
-- slashes, so for display consistency we just ensure they both have it.
showUrl :: AppComponentUrl -> String
showUrl = ensureTrailingSlash . AppComponentUrl.url
  where
    ensureTrailingSlash url = if last url == '/' then url else url ++ "/"
