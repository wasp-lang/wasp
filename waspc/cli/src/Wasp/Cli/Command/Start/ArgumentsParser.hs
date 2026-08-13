module Wasp.Cli.Command.Start.ArgumentsParser
  ( StartArgs (..),
    startArgsParser,
  )
where

import Network.Socket (PortNumber)
import qualified Options.Applicative as Opt
import Wasp.Cli.Util.PortArgument (portOption)
import qualified Wasp.Generator.ServerGenerator.Common as Server
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp

data StartArgs = StartArgs
  { clientPort :: PortNumber,
    serverPort :: PortNumber
  }
  deriving (Eq, Show)

startArgsParser :: Opt.Parser StartArgs
startArgsParser =
  StartArgs
    <$> portOption "client-port" "Port to run the client on" WebApp.defaultDevClientPort
    <*> portOption "server-port" "Port to run the server on" Server.defaultDevServerPort
