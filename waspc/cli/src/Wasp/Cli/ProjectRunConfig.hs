module Wasp.Cli.ProjectRunConfig
  ( defaultDevClientPort,
    defaultDevServerPort,
    makeDefaultDevProjectRunConfig,
    makeDevProjectRunConfig,
    makeProjectRunConfig,
    showRunConfigUrls,
  )
where

import Data.Maybe (fromMaybe)
import Network.Socket (PortNumber)
import Network.URI (URI, uriToString)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.Generator.WebAppGenerator.Common as WebAppG
import Wasp.Project.Db.Dev (findDevDatabaseUrl)
import Wasp.Project.RunConfig (ProjectRunConfig (..), makeLocalUrl)

defaultDevClientPort :: PortNumber
defaultDevClientPort = 3000

defaultDevServerPort :: PortNumber
defaultDevServerPort = 3001

makeDefaultDevProjectRunConfig :: AppSpec -> IO ProjectRunConfig
makeDefaultDevProjectRunConfig appSpec =
  makeDevProjectRunConfig appSpec (defaultDevClientPort, defaultDevServerPort) (Nothing, Nothing)

-- | Run config for the app in development, which connects it to the dev
-- database Wasp manages, if there is one.
makeDevProjectRunConfig :: AppSpec -> (PortNumber, PortNumber) -> (Maybe URI, Maybe URI) -> IO ProjectRunConfig
makeDevProjectRunConfig appSpec ports customUrls =
  makeProjectRunConfig appSpec ports customUrls <$> findDevDatabaseUrl appSpec

-- | Builds the run config from the ports the client and server listen on,
-- optionally, custom URLs to use instead of @http://localhost:<port>@, and
-- optionally, the database Wasp provides to the app.
makeProjectRunConfig :: AppSpec -> (PortNumber, PortNumber) -> (Maybe URI, Maybe URI) -> Maybe String -> ProjectRunConfig
makeProjectRunConfig appSpec ports@(clientPort, serverPort) (customClientUrl, customServerUrl) databaseUrl =
  ProjectRunConfig
    { clientUrl = fromMaybe localClientUrl customClientUrl,
      serverUrl = fromMaybe localServerUrl customServerUrl,
      clientPort,
      serverPort,
      databaseUrl
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
