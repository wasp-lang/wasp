module Wasp.Cli.ProjectRunConfig
  ( makeDefaultDevProjectRunConfig,
    makeProjectRunConfig,
    showRunConfigUrls,
  )
where

import Data.Maybe (fromMaybe)
import Network.Socket (PortNumber)
import Network.URI (URI (..), URIAuth (..))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import Wasp.Cli.AppComponentPorts (defaultDevClientPort, defaultDevServerPort)
import qualified Wasp.Generator.WebAppGenerator.Common as WebAppG
import Wasp.Project.RunConfig (ProjectRunConfig (..))

makeDefaultDevProjectRunConfig :: AppSpec -> ProjectRunConfig
makeDefaultDevProjectRunConfig appSpec =
  makeProjectRunConfig appSpec (defaultDevClientPort, Nothing) (defaultDevServerPort, Nothing)

-- | Construct the run config from the listening ports, falling back to local
-- URLs derived from them for the components the user didn't give a URL for.
makeProjectRunConfig :: AppSpec -> (PortNumber, Maybe URI) -> (PortNumber, Maybe URI) -> ProjectRunConfig
makeProjectRunConfig appSpec (clientPort, maybeClientUrl) (serverPort, maybeServerUrl) =
  ProjectRunConfig
    { -- A custom client URL already includes the base dir.
      clientUrl = fromMaybe (localUrl clientPort (Just $ WebAppG.getBaseDir appSpec)) maybeClientUrl,
      serverUrl = fromMaybe (localUrl serverPort Nothing) maybeServerUrl,
      clientPort,
      serverPort
    }
  where
    localUrl port path =
      URI
        { uriScheme = "http:",
          uriAuthority = Just $ localAuthority port,
          uriPath = maybe "" SP.fromAbsDirP path,
          uriQuery = "",
          uriFragment = ""
        }

showRunConfigUrls :: ProjectRunConfig -> String
showRunConfigUrls config =
  unlines
    [ " ℹ Client: " ++ showComponentUrl config.clientPort config.clientUrl,
      " ℹ Server: " ++ showComponentUrl config.serverPort config.serverUrl
    ]
  where
    showComponentUrl port url =
      ensureTrailingSlash (show url)
        ++ if url.uriAuthority == Just (localAuthority port)
          then ""
          else " (listening on local port " ++ show port ++ ")"

    -- The server and client URLs have different expectations for trailing
    -- slashes, so for display consistency we just ensure they both have it.
    ensureTrailingSlash url = if not (null url) && last url == '/' then url else url ++ "/"

localAuthority :: PortNumber -> URIAuth
localAuthority port = URIAuth "" "localhost" (":" ++ show port)
