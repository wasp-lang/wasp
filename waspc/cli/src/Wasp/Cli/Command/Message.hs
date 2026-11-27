module Wasp.Cli.Command.Message (cliSendMessageC) where

import Control.Monad.IO.Class (liftIO)
import Wasp.Cli.Command (Command, getConsole)
import Wasp.Cli.Console (Console (..))
import Wasp.Cli.Console.Reporter (Reporter (..))
import qualified Wasp.Message as Msg

cliSendMessageC :: Msg.Message -> Command ()
cliSendMessageC message = do
  console <- getConsole
  liftIO $ console.reporter.send message
