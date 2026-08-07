module Wasp.Cli.Command.Start.ArgumentsParser
  ( StartArgs (..),
    startArgsParser,
  )
where

import Network.Socket (PortNumber)
import qualified Options.Applicative as Opt
import Wasp.Cli.Util.PortArgument (servicePortsParser)
import Wasp.Project.PerService (PerService (..))

newtype StartArgs = StartArgs
  { ports :: PerService (Maybe PortNumber)
  }
  deriving (Eq, Show)

startArgsParser :: Opt.Parser StartArgs
startArgsParser = StartArgs <$> servicePortsParser
