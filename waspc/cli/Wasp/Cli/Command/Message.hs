module Wasp.Cli.Command.Message (cliSendMessageC) where

import Control.Monad.IO.Class (liftIO)
import Wasp.Cli.Command (Command)
import Wasp.Cli.Message (cliSendMessage)
import qualified Wasp.Message as Msg

cliSendMessageC :: Msg.Message -> Command ()
cliSendMessageC = liftIO . cliSendMessage
