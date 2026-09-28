module Wasp.Cli.AppComponentUrls
  ( makeDefaultUrls,
    makeAppComponentUrls,
  )
where

import Network.Socket (PortNumber)
import Network.URI (URI)
import Wasp.AppComponentUrl (AppComponentUrl (..))
import Wasp.AppSpec (AppSpec)
import Wasp.Cli.AppComponentPorts (defaultDevClientPort, defaultDevServerPort)
import qualified Wasp.Generator.WebAppGenerator.Common as WebAppG

makeDefaultUrls :: AppSpec -> (AppComponentUrl, AppComponentUrl)
makeDefaultUrls appSpec =
  makeAppComponentUrls appSpec (defaultDevClientPort, defaultDevServerPort) (Nothing, Nothing)

-- | Builds the client and server URLs from the ports they listen on and,
-- optionally, custom URLs at which they are reachable instead of
-- @http://localhost:<port>@.
makeAppComponentUrls :: AppSpec -> (PortNumber, PortNumber) -> (Maybe URI, Maybe URI) -> (AppComponentUrl, AppComponentUrl)
makeAppComponentUrls appSpec (clientPort, serverPort) (customClientUrl, customServerUrl) =
  ( maybe (Local {port = clientPort, path = Just $ WebAppG.getBaseDir appSpec}) (Custom clientPort) customClientUrl,
    maybe (Local {port = serverPort, path = Nothing}) (Custom serverPort) customServerUrl
  )
