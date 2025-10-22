module Wasp.Cli.Command.News
  ( news,
  )
where

import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Message as Msg

news :: Command ()
news = do
  cliSendMessageC $
    Msg.Info "News command - stub implementation"
