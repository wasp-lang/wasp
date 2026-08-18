module Wasp.Cli.AppComponentUrls
  ( makeDefaultUrls,
    showAppComponentUrls,
    makeDefaultDevClientUrl,
    defaultDevServerUrl,
    defaultDevClientPort,
    defaultDevServerPort,
  )
where

import Network.Socket (PortNumber)
import Wasp.AppComponentUrl (AppComponentUrl (..))
import qualified Wasp.AppComponentUrl as AppComponentUrl
import Wasp.AppSpec (AppSpec)
import qualified Wasp.Generator.WebAppGenerator.Common as WebAppG

makeDefaultUrls :: AppSpec -> (AppComponentUrl, AppComponentUrl)
makeDefaultUrls appSpec = (clientUrl, serverUrl)
  where
    clientUrl = makeDefaultDevClientUrl appSpec
    serverUrl = defaultDevServerUrl

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
