module Wasp.Cli.Command.Start.ArgumentsParser
  ( StartArgs (..),
    startArgsParser,
  )
where

import Network.Socket (PortNumber)
import qualified Options.Applicative as Opt
import Wasp.Cli.Util.PortArgument (portOption)
import qualified Wasp.Generator.Client as Client
import qualified Wasp.Generator.Server as Server

data StartArgs = StartArgs
  { clientPort :: PortNumber,
    serverPort :: PortNumber
  }
  deriving (Eq, Show)

startArgsParser :: Opt.Parser StartArgs
startArgsParser =
  StartArgs
    <$> portOption "client-port" "Port to run the client on" Client.defaultPort
    <*> portOption "server-port" "Port to run the server on" Server.defaultPort
