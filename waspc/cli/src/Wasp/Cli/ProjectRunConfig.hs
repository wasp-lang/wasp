module Wasp.Cli.ProjectRunConfig
  ( defaultDevClientPort,
    defaultDevServerPort,
    makeDefaultDevProjectRunConfig,
    makeProjectRunConfig,
    showRunConfigUrls,
  )
where

import Data.Maybe (fromMaybe)
import Network.Socket (PortNumber)
import Network.URI (URI, uriToString)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.Generator.WebAppGenerator.Common as WebAppG
import Wasp.Project.RunConfig (ProjectRunConfig (..), makeLocalUrl)

defaultDevClientPort :: PortNumber
defaultDevClientPort = 3000

defaultDevServerPort :: PortNumber
defaultDevServerPort = 3001

makeDefaultDevProjectRunConfig :: AppSpec -> ProjectRunConfig
makeDefaultDevProjectRunConfig appSpec =
  makeProjectRunConfig appSpec (defaultDevClientPort, defaultDevServerPort) (Nothing, Nothing)

-- | Builds the run config from the ports the client and server listen on and,
-- optionally, custom URLs to use instead of @http://localhost:<port>@.
makeProjectRunConfig :: AppSpec -> (PortNumber, PortNumber) -> (Maybe URI, Maybe URI) -> ProjectRunConfig
makeProjectRunConfig appSpec ports@(clientPort, serverPort) (customClientUrl, customServerUrl) =
  ProjectRunConfig
    { clientUrl = fromMaybe localClientUrl customClientUrl,
      serverUrl = fromMaybe localServerUrl customServerUrl,
      clientPort,
      serverPort
    }
  where
    (localClientUrl, localServerUrl) = makeLocalUrls appSpec ports

showRunConfigUrls :: AppSpec -> ProjectRunConfig -> String
showRunConfigUrls appSpec config =
  unlines
    [ showUrls "Client" config.clientUrl localClientUrl,
      showUrls "Server" config.serverUrl localServerUrl
    ]
  where
    (localClientUrl, localServerUrl) = makeLocalUrls appSpec (config.clientPort, config.serverPort)

    showUrls name url localUrl =
      concat
        [ " ℹ ",
          name,
          ": ",
          showUrl url,
          if url /= localUrl then " (local: " ++ showUrl localUrl ++ ")" else ""
        ]

    -- The server and client URLs have different expectations for trailing
    -- slashes, so for display consistency we just ensure they both have it.
    showUrl url = ensureTrailingSlash $ uriToString id url ""
    ensureTrailingSlash url = if not (null url) && last url == '/' then url else url ++ "/"

makeLocalUrls :: AppSpec -> (PortNumber, PortNumber) -> (URI, URI)
makeLocalUrls appSpec (clientPort, serverPort) =
  ( makeLocalUrl clientPort (Just $ WebAppG.getBaseDir appSpec),
    makeLocalUrl serverPort Nothing
  )
