module Wasp.Cli.AppComponents
  ( makeDevRunConfigs,
    showAppUrls,
  )
where

import Network.Socket (PortNumber)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.Generator.Client as Client
import qualified Wasp.Generator.Server as Server

-- | Makes the run configs for a development-style run of the app and wires
-- them together: each component learns the other's URL, so the client knows
-- where the API is and the server knows which origin to allow through CORS.
--
-- Convention: wherever the two components appear together, the client comes
-- first (as in this result tuple).
makeDevRunConfigs :: AppSpec -> PortNumber -> PortNumber -> (Client.ClientRunConfig, Server.ServerRunConfig)
makeDevRunConfigs spec clientPort serverPort =
  ( client {Client.serverUrl = Just (Server.url server)},
    server {Server.clientUrl = Just (Client.url client)}
  )
  where
    client = Client.make spec clientPort
    server = Server.make serverPort

showAppUrls :: Client.ClientRunConfig -> Server.ServerRunConfig -> String
showAppUrls client server =
  unlines
    [ " ℹ Client: " ++ ensureTrailingSlash (Client.url client),
      " ℹ Server: " ++ ensureTrailingSlash (Server.url server)
    ]
  where
    -- The server and client URLs have different expectations for trailing
    -- slashes, so for display consistency we just ensure they both have it.
    ensureTrailingSlash url = if last url == '/' then url else url ++ "/"
