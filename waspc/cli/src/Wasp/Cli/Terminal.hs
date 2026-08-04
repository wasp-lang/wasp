module Wasp.Cli.Terminal
  ( title,
    asWaspMessage,
    asWaspStartMessage,
    asWaspSuccessMessage,
    asWaspFailureMessage,
    asWaspWarningMessage,
    asWaspAppUrlsMessage,
  )
where

import Data.Foldable (toList)
import Data.List (intercalate)
import Wasp.Project.PerService (PerService, names)
import Wasp.Util (toUpperFirst)
import qualified Wasp.Util.Terminal as Term

title :: String -> String
title = Term.applyStyles [Term.Bold]

asWaspMessage :: String -> String
asWaspMessage = waspMessageWithEmoji ""

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

-- | Tells the user where each of the app's parts is running. Wasp picks the
-- ports itself, so users can't know them in advance.
asWaspAppUrlsMessage :: PerService String -> String
asWaspAppUrlsMessage urls = intercalate "\n" . toList $ makeUrlLine <$> names <*> urls
  where
    makeUrlLine name url = " ℹ " ++ toUpperFirst name ++ ": " ++ url

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
