module Wasp.Cli.Command.Message
  ( cliSendMessageC,
    cliWriteRawOutputC,
    cliWriteDataC,
    runTaskC,
    runTaskWithResultC,
    handOverTerminalC,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Wasp.Cli.Command (Command, getConsole)
import Wasp.Cli.Console (Console (..))
import Wasp.Cli.Console.Reporter (Reporter (..), TaskEnd, runTaskWithFixedTexts)
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

cliWriteDataC :: String -> Command ()
cliWriteDataC text = do
  console <- getConsole
  liftIO $ console.writeData text

-- | See 'runTaskWithStatusReport'.
runTaskC :: String -> String -> String -> Command a -> Command a
runTaskC runningText successText failedText task = do
  console <- getConsole
  runTaskWithFixedTexts console.reporter runningText successText failedText task

-- | Like 'runTaskC', for a task whose result says how it ended.
runTaskWithResultC :: String -> (a -> TaskEnd) -> String -> Command a -> Command a
runTaskWithResultC runningText describeTaskEnd failedText task = do
  console <- getConsole
  runTaskWithStatusReport console.reporter runningText describeTaskEnd failedText task

-- | See 'handOverTerminal'.
handOverTerminalC :: Command a -> Command a
handOverTerminalC action = do
  console <- getConsole
  handOverTerminal console action
