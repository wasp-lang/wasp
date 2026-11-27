module Wasp.Cli.Console.Reporter
  ( Reporter (..),
    cliSendMessage,
  )
where

import qualified Wasp.Message as Msg
import qualified Wasp.Util.TextStyle as TextStyle

-- | How Wasp says things to the user.
newtype Reporter = Reporter
  { send :: Msg.Message -> IO ()
  }

-- | Send a message using the CLI
cliSendMessage :: Msg.SendMessage
cliSendMessage (Msg.Info msg) =
  waspSays msg
cliSendMessage (Msg.Start msg) =
  waspSays $ asWaspStartMessage msg
cliSendMessage (Msg.Success msg) =
  waspSays $ asWaspSuccessMessage msg
cliSendMessage (Msg.Failure title msg) =
  waspScreams $ asWaspFailureMessage (title ++ ":") ++ msg
cliSendMessage (Msg.Warning title msg) =
  waspWarns $ asWaspWarningMessage (title ++ ":") ++ msg

waspSays :: String -> IO ()
waspSays what = putStrLn $ TextStyle.applyStyles [TextStyle.Yellow] what

waspWarns :: String -> IO ()
waspWarns what = putStrLn $ TextStyle.applyStyles [TextStyle.Magenta] what

waspScreams :: String -> IO ()
waspScreams what = putStrLn $ TextStyle.applyStyles [TextStyle.Red] what

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
