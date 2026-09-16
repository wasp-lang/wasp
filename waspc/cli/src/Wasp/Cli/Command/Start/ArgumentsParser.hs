module Wasp.Cli.Command.Start.ArgumentsParser
  ( StartArgs (..),
    startArgsParser,
  )
where

import Network.Socket (PortNumber)
import qualified Options.Applicative as Opt
import Wasp.Cli.Util.PortArgument (portOption)

data StartArgs = StartArgs
  { clientPort :: Maybe PortNumber,
    serverPort :: Maybe PortNumber
  }
  deriving (Eq, Show)

startArgsParser :: Opt.Parser StartArgs
startArgsParser =
  StartArgs
    <$> portOption "client-port" "Port to run the client on"
    <*> portOption "server-port" "Port to run the server on"
