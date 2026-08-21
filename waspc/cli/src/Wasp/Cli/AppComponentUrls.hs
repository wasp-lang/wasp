module Wasp.Cli.AppComponentUrls
  ( makeDevUrls,
    makeDevClientUrl,
    makeDevServerUrl,
  )
where

import Network.Socket (PortNumber)
import Wasp.AppComponentUrl (AppComponentUrl (..))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.Generator.WebAppGenerator.Common as WebAppG

makeDevUrls :: AppSpec -> (PortNumber, PortNumber) -> (AppComponentUrl, AppComponentUrl)
makeDevUrls appSpec (clientPort, serverPort) = (clientUrl, serverUrl)
  where
    clientUrl = makeDevClientUrl appSpec clientPort
    serverUrl = makeDevServerUrl serverPort

makeDevClientUrl :: AppSpec -> PortNumber -> AppComponentUrl
makeDevClientUrl spec port =
  Local {port = port, path = Just $ WebAppG.getBaseDir spec}

makeDevServerUrl :: PortNumber -> AppComponentUrl
makeDevServerUrl port =
  Local {port = port, path = Nothing}
