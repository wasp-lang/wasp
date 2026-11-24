-- | The reporter that writes to the terminal. Only "Wasp.Cli.Console" uses it,
-- when it builds the console.
module Wasp.Cli.Console.Internal.TerminalReporter
  ( makeTerminalReporter,
  )
where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Wasp.Cli.Console.Internal.Terminal as Terminal
import Wasp.Cli.Console.Reporter (Reporter (..))
import qualified Wasp.Message as Msg
import qualified Wasp.Util.TextStyle as TextStyle

makeTerminalReporter :: Reporter
makeTerminalReporter =
  Reporter
    { send = sendMessage,
      runTaskWithStatusReport = runTaskBySendingMessages sendMessage
    }
  where
    sendMessage = Terminal.writeLine . renderMessage

-- | Reports a task with a start message and a success message. An empty
-- running text means that there is no start message.
runTaskBySendingMessages :: (MonadIO m) => (Msg.Message -> IO ()) -> String -> String -> m a -> m a
runTaskBySendingMessages sendMessage runningText successText task = do
  unless (null runningText) $ liftIO $ sendMessage $ Msg.Start runningText
  result <- task
  liftIO $ sendMessage $ Msg.Success successText
  return result

renderMessage :: Msg.Message -> String
renderMessage (Msg.Info msg) =
  waspSays msg
renderMessage (Msg.Start msg) =
  waspSays $ asWaspStartMessage msg
renderMessage (Msg.Success msg) =
  waspSays $ asWaspSuccessMessage msg
renderMessage (Msg.Failure title msg) =
  waspScreams $ asWaspFailureMessage (title ++ ":") ++ msg
renderMessage (Msg.Warning title msg) =
  waspWarns $ asWaspWarningMessage (title ++ ":") ++ msg

waspSays :: String -> String
waspSays what = TextStyle.applyStyles [TextStyle.Yellow] what

waspWarns :: String -> String
waspWarns what = TextStyle.applyStyles [TextStyle.Magenta] what

waspScreams :: String -> String
waspScreams what = TextStyle.applyStyles [TextStyle.Red] what

asWaspStartMessage :: String -> String
asWaspStartMessage = waspMessageWithEmoji "🐝"

asWaspSuccessMessage :: String -> String
asWaspSuccessMessage = waspMessageWithEmoji "✅"

asWaspWarningMessage :: String -> String
asWaspWarningMessage str = concat ["\n", waspMessageWithEmoji "👀" errorStr, "\n"]
  where
    errorStr = "[Warning] " ++ str

asWaspFailureMessage :: String -> String
-- Add a bit more padding on errors for more pronounced
-- visibility and better display of any following error context.
asWaspFailureMessage str = concat ["\n", waspMessageWithEmoji "❌" errorStr, "\n"]
  where
    errorStr = "[Error] " ++ str

waspMessageWithEmoji :: String -> String -> String
waspMessageWithEmoji emoji message = concat ["\n", prefix, " ", message, " ", suffix, "\n"]
  where
    prefix = emoji ++ " ---"
    prefixAndMessageLength = length prefix + length message
    idealLength = 80
    -- Pad suffix until returned message is the ideal length. However, if we have to go
    -- beyond ideal length due to input length, just use 3 at the end to match the prefix.
    rightPadLength = max 3 (idealLength - prefixAndMessageLength)
    suffix = concat (replicate rightPadLength "-")
