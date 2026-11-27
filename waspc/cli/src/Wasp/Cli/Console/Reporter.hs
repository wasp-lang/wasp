module Wasp.Cli.Console.Reporter
  ( Reporter (..),
    renderMessage,
  )
where

import qualified Wasp.Message as Msg
import qualified Wasp.Util.TextStyle as TextStyle

-- | How Wasp says things to the user.
newtype Reporter = Reporter
  { send :: Msg.Message -> IO ()
  }

renderMessage :: Msg.Message -> String
renderMessage = \case
  Msg.Info msg -> waspSays msg
  Msg.Start msg -> waspSays $ asWaspStartMessage msg
  Msg.Success msg -> waspSays $ asWaspSuccessMessage msg
  Msg.Failure title msg -> waspScreams $ asWaspFailureMessage (title ++ ":") ++ msg
  Msg.Warning title msg -> waspWarns $ asWaspWarningMessage (title ++ ":") ++ msg

waspSays :: String -> String
waspSays = TextStyle.applyStyles [TextStyle.Yellow]

waspWarns :: String -> String
waspWarns = TextStyle.applyStyles [TextStyle.Magenta]

waspScreams :: String -> String
waspScreams = TextStyle.applyStyles [TextStyle.Red]

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
