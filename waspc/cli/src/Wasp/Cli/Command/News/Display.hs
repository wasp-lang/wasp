{-# LANGUAGE OverloadedRecordDot #-}

module Wasp.Cli.Command.News.Display (displayNewsEntry) where

import Data.Time (defaultTimeLocale, formatTime)
import Wasp.Cli.Command.News.Common (NewsEntry (..), NewsLevel (..))
import Wasp.Util (indent, wrapString)
import qualified Wasp.Util.Terminal as Term

displayNewsEntry :: NewsEntry -> String
displayNewsEntry newsEntry =
  Term.applyStyles [Term.Bold] newsEntry.title
    <> " "
    <> Term.applyStyles [Term.Bold] (replicate dotCount '.')
    <> " "
    <> Term.applyStyles [Term.Yellow, Term.Bold] dateText
    <> "\n"
    <> showLevelInColor newsEntry.level
    <> "\n"
    <> Term.applyStyles [Term.Grey] (indent 2 $ wrapString (maxColumns - 2) newsEntry.body)
  where
    dateText = formatTime defaultTimeLocale "%Y-%m-%d" newsEntry.publishedAt
    dotCount = max minDotsCount (maxColumns - length newsEntry.title - length dateText - 2)
    maxColumns = 80
    minDotsCount = 5

showLevelInColor :: NewsLevel -> String
showLevelInColor newsLevel = case newsLevel of
  High -> Term.applyStyles [Term.Red] "High"
  Moderate -> Term.applyStyles [Term.Yellow] "Moderate"
  Low -> Term.applyStyles [Term.Blue] "Low"
