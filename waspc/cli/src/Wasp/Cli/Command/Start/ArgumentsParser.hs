module Wasp.Cli.Command.Start.ArgumentsParser
  ( StartArgs (..),
    startArgsParser,
  )
where

import Network.Socket (PortNumber)
import Network.URI (URI)
import qualified Options.Applicative as Opt
import Wasp.Cli.Util.PortArgument (portOption)
import Wasp.Cli.Util.UrlArgument (urlOption)

data StartArgs = StartArgs
  { clientPort :: Maybe PortNumber,
    serverPort :: Maybe PortNumber,
    clientUrl :: Maybe URI,
    serverUrl :: Maybe URI
  }
  deriving (Eq, Show)

startArgsParser :: Opt.Parser StartArgs
startArgsParser =
  StartArgs
    <$> portOption "client-port" "Port to run the client on"
    <*> portOption "server-port" "Port to run the server on"
    <*> urlOption
      "client-url"
      "Public URL the client is reached at. http://localhost:<client-port> by default."
    <*> urlOption
      "server-url"
      "Public URL the server is reached at. http://localhost:<server-port> by default."
