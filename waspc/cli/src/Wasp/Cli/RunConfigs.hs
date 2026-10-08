module Wasp.Cli.RunConfigs
  ( makeDefaultDevRunConfigs,
    makeRunConfigs,
    makeDevDbRunConfig,
    showRunConfigUrls,
  )
where

import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Wasp.AppComponentUrl (AppComponentUrl (..))
import qualified Wasp.AppComponentUrl as AppComponentUrl
import Wasp.AppSpec (AppSpec (..))
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Cli.AppComponentUrls (makeDefaultUrls)
import Wasp.Db.RunConfig
import Wasp.Generator.ServerGenerator.RunConfig (makeServerRunConfig)
import qualified Wasp.Generator.ServerGenerator.RunConfig as ServerRunConfig
import Wasp.Generator.WebAppGenerator.RunConfig (makeWebAppRunConfig)
import qualified Wasp.Generator.WebAppGenerator.RunConfig as WebAppRunConfig
import Wasp.Project.Db (databaseUrlEnvVarName)
import Wasp.RunConfig (RunConfigs (..))

makeDefaultDevRunConfigs :: AppSpec -> IO RunConfigs
makeDefaultDevRunConfigs appSpec =
  makeRunConfigs (makeDefaultUrls appSpec) <$> makeDevDbRunConfig appSpec

makeDevDbRunConfig :: AppSpec -> IO DbRunConfig
makeDevDbRunConfig appSpec = do
  environmentUrl <- lookupEnv databaseUrlEnvVarName
  let defaultConfig = fromMaybe (DbRunConfig (ASV.getValidDbSystem appSpec) Unconfigured) appSpec.devDbRunConfig
      serverDotEnvUrl = lookup databaseUrlEnvVarName appSpec.devEnvVarsServer
  return $ resolveDevConnection environmentUrl serverDotEnvUrl defaultConfig

makeRunConfigs :: (AppComponentUrl, AppComponentUrl) -> DbRunConfig -> RunConfigs
makeRunConfigs (clientUrl, serverUrl) database = RunConfigs clientRunConfig serverRunConfig database
  where
    clientRunConfig = makeWebAppRunConfig clientUrl (AppComponentUrl.url serverUrl)
    serverRunConfig = makeServerRunConfig serverUrl (AppComponentUrl.url clientUrl)

showRunConfigUrls :: RunConfigs -> String
showRunConfigUrls configs =
  unlines
    [ showUrls "Client" configs.client.url,
      showUrls "Server" configs.server.url
    ]
  where
    showUrls name appComponentUrl =
      concat
        [ " ℹ ",
          name,
          ": ",
          showUrl appComponentUrl,
          showLocalUrl appComponentUrl
        ]

    showUrl AppComponentUrl {url} = ensureTrailingSlash url

    showLocalUrl AppComponentUrl {url, localUrl} =
      if url /= localUrl
        then " (local: " ++ ensureTrailingSlash localUrl ++ ")"
        else ""

    -- The server and client URLs have different expectations for trailing
    -- slashes, so for display consistency we just ensure they both have it.
    ensureTrailingSlash url = if last url == '/' then url else url ++ "/"
