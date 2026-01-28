{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Wasp.Cli.Command.News.Display (showNewsEntry) where

import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime)
import NeatInterpolation (trimming)
import Wasp.Cli.Command.News.Core (NewsEntry (..), NewsLevel (..))
import Wasp.Util (indent, wrapString)
import qualified Wasp.Util.Terminal as Term

showNewsEntry :: NewsEntry -> String
showNewsEntry newsEntry =
  -- The formatting here affects the validation in the wasp-news repo.
  -- If you change something, update it there too.
  --
  -- Permalink at time of writing:
  -- https://github.com/wasp-lang/wasp-news/blob/2335ee25f23de1664321d84ca63aba4331722c9d/scripts/validate-news-format.js#L16
  T.unpack
    [trimming|
    ${title} ${dots} ${publishedAt}
    ${level}
    ${body}
  |]
  where
    title = T.pack $ Term.applyStyles [Term.Bold] newsEntry.title
    dots = T.pack $ replicate dotCount '.'
    publishedAt = T.pack $ Term.applyStyles [Term.Bold, Term.Yellow] date
    level = T.pack $ showNewsLevel newsEntry.level
    body = T.pack $ Term.applyStyles [Term.Grey] $ indent indentSize $ wrapString (maxColumns - indentSize) newsEntry.body

    date = formatTime defaultTimeLocale "%Y-%m-%d" newsEntry.publishedAt

    dotCount = max minDotsCount $ maxColumns - length newsEntry.title - length date - spacesInSeparatorCount
    spacesInSeparatorCount = 2
    minDotsCount = 5

    maxColumns = 80
    indentSize = 2

showNewsLevel :: NewsLevel -> String
showNewsLevel newsLevel = case newsLevel of
  Critical -> Term.applyStyles [Term.Red] "Critical"
  Important -> Term.applyStyles [Term.Yellow] "Important"
  Info -> Term.applyStyles [Term.Blue] "Info"
