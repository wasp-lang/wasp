{-# LANGUAGE OverloadedRecordDot #-}

module Wasp.Cli.Command.News.Display (printNewsEntry) where

import Data.Time (defaultTimeLocale, formatTime)
import Wasp.Cli.Command.News.Common (NewsEntry (..), NewsLevel (..))
import Wasp.Util (indent, wrapString)
import qualified Wasp.Util.Terminal as Term

printNewsEntry :: NewsEntry -> IO ()
printNewsEntry entry = do
  putStrLn ""
  putStrLn $
    Term.applyStyles [Term.Bold] entry.title
      <> " "
      <> Term.applyStyles [Term.Bold] (replicate dotCount '.')
      <> " "
      <> Term.applyStyles [Term.Yellow, Term.Bold] dateText
  putStrLn $
    showLevelInColor entry.level
      <> "\n"
      <> Term.applyStyles [Term.Grey] (indent 2 $ wrapString (maxColumns - 2) entry.body)
  where
    dateText = formatTime defaultTimeLocale "%Y-%m-%d" (publishedAt entry)
    dotCount = max minDotsCount (maxColumns - length entry.title - length dateText - 2)
    maxColumns = 80
    minDotsCount = 5

showLevelInColor :: NewsLevel -> String
showLevelInColor newsLevel = case newsLevel of
  High -> Term.applyStyles [Term.Red] "High"
  Moderate -> Term.applyStyles [Term.Yellow] "Moderate"
  Low -> Term.applyStyles [Term.Blue] "Low"
