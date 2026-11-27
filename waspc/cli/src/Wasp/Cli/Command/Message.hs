module Wasp.Cli.Command.Message
  ( cliSendMessageC,
    cliWriteRawOutputC,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Wasp.Cli.Command (Command, getConsole)
import Wasp.Cli.Console (Console (..))
import Wasp.Cli.Console.Reporter (Reporter (..))
import qualified Wasp.Message as Msg
import Wasp.Util.IO.Process (OutputStream)

cliSendMessageC :: Msg.Message -> Command ()
cliSendMessageC message = do
  console <- getConsole
  liftIO $ console.reporter.send message

cliWriteRawOutputC :: OutputStream -> String -> Command ()
cliWriteRawOutputC outputStream text = do
  console <- getConsole
  liftIO $ console.writeRawOutput outputStream $ T.pack text
