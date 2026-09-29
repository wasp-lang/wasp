module Wasp.Cli.Command.Start.ArgumentsParser
  ( StartArgs (..),
    startArgsParser,
  )
where

import Network.Socket (PortNumber)
import qualified Options.Applicative as Opt
import Wasp.Cli.Util.PortArgument (portOption)
import Wasp.Cli.Util.UrlArgument (UrlArgument, urlOption)

data StartArgs = StartArgs
  { clientPort :: Maybe PortNumber,
    serverPort :: Maybe PortNumber,
    clientUrl :: Maybe UrlArgument,
    serverUrl :: Maybe UrlArgument
  }
  deriving (Eq, Show)

startArgsParser :: Opt.Parser StartArgs
startArgsParser =
  StartArgs
    <$> portOption "client-port" "Port to run the client on"
    <*> portOption "server-port" "Port to run the server on"
    <*> urlOption "client-url" "URL at which the client is reachable"
    <*> urlOption "server-url" "URL at which the server is reachable"
