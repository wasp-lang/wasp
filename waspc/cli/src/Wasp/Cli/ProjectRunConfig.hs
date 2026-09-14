module Wasp.Cli.ProjectRunConfig
  ( makeDefaultDevProjectRunConfig,
    makeProjectRunConfig,
    showRunConfigUrls,
  )
where

import Network.Socket (PortNumber)
import Network.URI (URI (..), URIAuth (..))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import Wasp.Cli.AppComponentPorts (defaultDevClientPort, defaultDevServerPort)
import qualified Wasp.Generator.WebAppGenerator.Common as WebAppG
import Wasp.Project.RunConfig (ProjectRunConfig (..))

makeDefaultDevProjectRunConfig :: AppSpec -> ProjectRunConfig
makeDefaultDevProjectRunConfig appSpec =
  makeProjectRunConfig appSpec (defaultDevClientPort, defaultDevServerPort)

-- | Construct local URLs from the listening ports, keeping them in sync.
makeProjectRunConfig :: AppSpec -> (PortNumber, PortNumber) -> ProjectRunConfig
makeProjectRunConfig appSpec (clientPort, serverPort) =
  ProjectRunConfig
    { clientUrl = localUrl clientPort (Just $ WebAppG.getBaseDir appSpec),
      serverUrl = localUrl serverPort Nothing,
      clientPort,
      serverPort
    }
  where
    localUrl port path =
      URI
        { uriScheme = "http:",
          uriAuthority = Just $ URIAuth "" "localhost" (":" ++ show port),
          uriPath = maybe "" SP.fromAbsDirP path,
          uriQuery = "",
          uriFragment = ""
        }

showRunConfigUrls :: ProjectRunConfig -> String
showRunConfigUrls config =
  unlines
    [ " ℹ Client: " ++ ensureTrailingSlash (show config.clientUrl),
      " ℹ Server: " ++ ensureTrailingSlash (show config.serverUrl)
    ]
  where
    -- The server and client URLs have different expectations for trailing
    -- slashes, so for display consistency we just ensure they both have it.
    ensureTrailingSlash url = if not (null url) && last url == '/' then url else url ++ "/"
