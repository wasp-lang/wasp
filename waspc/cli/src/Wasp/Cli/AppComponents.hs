module Wasp.Cli.AppComponents
  ( makeDevRunConfigs,
  )
where

import Wasp.AppSpec (AppSpec)
import qualified Wasp.Generator.Client as Client
import qualified Wasp.Generator.Server as Server

-- | Makes the run configs for a development-style run of the app and wires
-- them together: each component learns the other's URL, so the client knows
-- where the API is and the server knows which origin to allow through CORS.
--
-- Convention: wherever the two components appear together, the client comes
-- first (as in this result tuple).
makeDevRunConfigs :: AppSpec -> (Client.ClientRunConfig, Server.ServerRunConfig)
makeDevRunConfigs spec =
  ( client {Client.serverUrl = Just (Server.url server)},
    server {Server.clientUrl = Just (Client.url client)}
  )
  where
    client = Client.make spec
    server = Server.make
