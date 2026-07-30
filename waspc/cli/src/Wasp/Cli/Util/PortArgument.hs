-- | Choosing which ports 'wasp start' should run the client and the server on.
--
-- The ports come from the @--client-port@ and @--server-port@ flags, and default
-- to the ports Wasp apps have always used when the user doesn't pass them.
module Wasp.Cli.Util.PortArgument
  ( defaultAppPorts,
    appPortsParser,
    portOption,
  )
where

import Network.Socket (PortNumber)
import qualified Options.Applicative as Opt
import Wasp.Project.Apps (Apps (..))
import qualified Wasp.Project.Apps as Apps

appPortsParser :: Opt.Parser (Apps PortNumber)
appPortsParser =
  sequenceA $
    liftA2
      (\name defaultPort -> portOption (name ++ "-port") ("Port to run the " ++ name ++ " on") defaultPort)
      Apps.names
      defaultAppPorts

portOption :: String -> String -> PortNumber -> Opt.Parser PortNumber
portOption optionName helpText defaultPort =
  Opt.option
    (Opt.auto >>= rejectAnyPort)
    ( Opt.long optionName
        <> Opt.metavar "PORT"
        <> Opt.help helpText
        <> Opt.value defaultPort
        <> Opt.showDefault
    )
  where
    -- Reading into a 'PortNumber' already rejects anything outside 1-65535,
    -- except for 0, which means "let the OS pick a port". We can't work with
    -- that, since we have to tell the other side where this one is running.
    rejectAnyPort 0 = Opt.readerError "0 is not a valid port"
    rejectAnyPort port = return port

-- | The ports an app runs on when the user doesn't choose any. 3000 and 3001
-- are the ports people expect from a Wasp app.
defaultAppPorts :: Apps PortNumber
defaultAppPorts =
  Apps
    { client = 3000,
      server = 3001
    }
