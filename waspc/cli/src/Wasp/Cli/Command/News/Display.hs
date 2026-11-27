module Wasp.Cli.Command.News.Display (showNewsEntry) where

import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime)
import NeatInterpolation (trimming)
import Wasp.Cli.Command.News.Core (NewsEntry (..), NewsLevel (..))
import Wasp.Util (indent, wrapString)
import qualified Wasp.Util.TextStyle as TextStyle

showNewsEntry :: NewsEntry -> String
showNewsEntry newsEntry =
  -- The formatting here affects the validation in the wasp-news repo.
  -- If you change something, update it there too.
  --
  -- Permalink at time of writing:
  -- https://github.com/wasp-lang/wasp-news/blob/2335ee25f23de1664321d84ca63aba4331722c9d/scripts/validate-news-format.js#L16
  T.unpack
    [trimming|
    ${title}${separator}${publishedAt}
    ${level}
    ${body}
  |]
  where
    title = T.pack $ TextStyle.applyStyles [TextStyle.Bold] newsEntry.title
    publishedAt = T.pack $ TextStyle.applyStyles [TextStyle.Bold, TextStyle.Yellow] date
    level = T.pack $ showNewsLevel newsEntry.level
    body = T.pack $ TextStyle.applyStyles [TextStyle.Grey] $ indent indentSize $ wrapString (maxColumns - indentSize) newsEntry.body
    separator =
      T.pack $
        " "
          <> replicate (max minDotsCount (maxColumns - length newsEntry.title - length date - 2)) '.'
          <> " "

    date = formatTime defaultTimeLocale "%Y-%m-%d" newsEntry.publishedAt

    minDotsCount = 5
    maxColumns = 80
    indentSize = 2

showNewsLevel :: NewsLevel -> String
showNewsLevel newsLevel = case newsLevel of
  Critical -> TextStyle.applyStyles [TextStyle.Red] "Critical"
  Important -> TextStyle.applyStyles [TextStyle.Yellow] "Important"
  Info -> TextStyle.applyStyles [TextStyle.Blue] "Info"
