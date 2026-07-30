module Wasp.Cli.Command.Start.ArgumentsParser
  ( StartArgs (..),
    startArgsParser,
  )
where

import Network.Socket (PortNumber)
import qualified Options.Applicative as Opt
import Wasp.Cli.Util.PortArgument (appPortsParser)
import Wasp.Project.Apps (Apps (..))

newtype StartArgs = StartArgs
  { ports :: Apps (Maybe PortNumber)
  }
  deriving (Eq, Show)

startArgsParser :: Opt.Parser StartArgs
startArgsParser = StartArgs <$> appPortsParser
