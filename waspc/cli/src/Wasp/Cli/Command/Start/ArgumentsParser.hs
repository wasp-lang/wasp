module Wasp.Cli.Command.Start.ArgumentsParser
  ( StartArgs (..),
    startArgsParser,
  )
where

import Network.Socket (PortNumber)
import qualified Options.Applicative as Opt
import Wasp.Cli.AppComponentUrls (defaultDevClientPort, defaultDevServerPort)
import Wasp.Cli.Util.PortArgument (portOption)

data StartArgs = StartArgs
  { clientPort :: PortNumber,
    serverPort :: PortNumber
  }
  deriving (Eq, Show)

startArgsParser :: Opt.Parser StartArgs
startArgsParser =
  StartArgs
    <$> portOption "client-port" "Port to run the client on" defaultDevClientPort
    <*> portOption "server-port" "Port to run the server on" defaultDevServerPort
