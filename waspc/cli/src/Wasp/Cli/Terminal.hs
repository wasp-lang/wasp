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
import Data.List (intercalate, isSuffixOf)
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
    makeUrlLine name url = " ℹ " ++ toUpperFirst name ++ ": " ++ ensureTrailingSlash url

    -- The client's URL ends with a slash (it's the app's base directory) while
    -- the server's doesn't, and showing the pair like that looks like a typo.
    -- We add the slash rather than drop it because a client with a custom base
    -- directory only serves the app on the path that ends with one.
    ensureTrailingSlash url = if "/" `isSuffixOf` url then url else url ++ "/"

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
