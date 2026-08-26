module Wasp.Cli.AppComponentUrls
  ( makeDefaultUrls,
    makeDefaultDevClientUrl,
    defaultDevServerUrl,
  )
where

import Wasp.AppComponentUrl (AppComponentUrl (..))
import Wasp.AppSpec (AppSpec)
import Wasp.Cli.AppComponentPorts (defaultDevClientPort, defaultDevServerPort)
import qualified Wasp.Generator.WebAppGenerator.Common as WebAppG

makeDefaultUrls :: AppSpec -> (AppComponentUrl, AppComponentUrl)
makeDefaultUrls appSpec = (clientUrl, serverUrl)
  where
    clientUrl = makeDefaultDevClientUrl appSpec
    serverUrl = defaultDevServerUrl

makeDefaultDevClientUrl :: AppSpec -> AppComponentUrl
makeDefaultDevClientUrl spec =
  Local {port = defaultDevClientPort, path = Just $ WebAppG.getBaseDir spec}

defaultDevServerUrl :: AppComponentUrl
defaultDevServerUrl =
  Local {port = defaultDevServerPort, path = Nothing}
