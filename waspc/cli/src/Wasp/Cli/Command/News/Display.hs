{-# LANGUAGE OverloadedRecordDot #-}

module Wasp.Cli.Command.News.Display (showNewsEntry) where

import Data.List (intercalate)
import Data.Time (defaultTimeLocale, formatTime)
import Wasp.Cli.Command.News.Common (NewsEntry (..), NewsLevel (..))
import Wasp.Util (indent, wrapString)
import qualified Wasp.Util.Terminal as Term

showNewsEntry :: NewsEntry -> String
showNewsEntry newsEntry =
  intercalate "\n" [headerLine, levelLine, bodyBlock]
  where
    headerLine =
      Term.applyStyles [Term.Bold] newsEntry.title
        <> " "
        <> Term.applyStyles [Term.Bold] (replicate dotCount '.')
        <> " "
        <> Term.applyStyles [Term.Yellow, Term.Bold] date

    levelLine = showNewsLevel newsEntry.level

    bodyBlock = Term.applyStyles [Term.Grey] $ indent indentSize $ wrapString (maxColumns - indentSize) newsEntry.body

    dotCount = max minDotsCount (maxColumns - length newsEntry.title - length date - 2)
    date = formatTime defaultTimeLocale "%Y-%m-%d" newsEntry.publishedAt

    maxColumns = 80
    minDotsCount = 5
    indentSize = 2

showNewsLevel :: NewsLevel -> String
showNewsLevel newsLevel = case newsLevel of
  Critical -> Term.applyStyles [Term.Red] "Critical"
  Important -> Term.applyStyles [Term.Yellow] "Important"
  Info -> Term.applyStyles [Term.Blue] "Info"
