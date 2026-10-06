module Wasp.Cli.Command.Start.ArgumentsParser
  ( StartArgs (..),
    startArgsParser,
  )
where

import Network.Socket (PortNumber)
import Network.URI (URI)
import qualified Options.Applicative as Opt
import Wasp.Cli.Command.Db.ArgumentsParser (StartDbArgs, startDbArgsParser)
import Wasp.Cli.Util.HttpUrlArgument (httpUrlParser)
import Wasp.Cli.Util.PortArgument (portParser)

data StartArgs = StartArgs
  { clientPort :: Maybe PortNumber,
    serverPort :: Maybe PortNumber,
    clientUrl :: Maybe URI,
    serverUrl :: Maybe URI,
    dbArgs :: StartDbArgs
  }
  deriving (Eq, Show)

startArgsParser :: Opt.Parser StartArgs
startArgsParser =
  StartArgs
    <$> Opt.optional (portParser "client-port" "Port to run the client on")
    <*> Opt.optional (portParser "server-port" "Port to run the server on")
    <*> Opt.optional (httpUrlParser "client-url" "URL at which the client is reachable")
    <*> Opt.optional (httpUrlParser "server-url" "URL at which the server is reachable")
    <*> startDbArgsParser
