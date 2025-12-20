{-# LANGUAGE OverloadedRecordDot #-}

module Wasp.Cli.Command.News.Display (printNewsEntry) where

import Data.Time (defaultTimeLocale, formatTime)
import Wasp.Cli.Command.News.Common (NewsEntry (..))
import Wasp.Util (indent)
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
      <> Term.applyStyles [Term.Grey] (indent 2 $ wrapText (maxColumns - 2) entry.body)
  where
    dateText = formatTime defaultTimeLocale "%Y-%m-%d" (publishedAt entry)
    dotCount = max minDotsCount (maxColumns - length entry.title - length dateText - 2)
    maxColumns = 80
    minDotsCount = 5

showLevelInColor :: String -> String
showLevelInColor newsLevel = styleLevel newsLevel
  where
    styleLevel = case newsLevel of
      "high" -> Term.applyStyles [Term.Red]
      "moderate" -> Term.applyStyles [Term.Yellow]
      "low" -> Term.applyStyles [Term.Blue]
      _ -> error "Invalid"

wrapText :: Int -> String -> String
wrapText maxLen text = go 0 [] (words text)
  where
    go :: Int -> [String] -> [String] -> String
    go _ wrappedTokens [] = concat $ drop 1 $ reverse wrappedTokens
    go lastLineLen wrappedTokens (nextWord : wordsRest) =
      let lastLineLen' = lastLineLen + 1 + length nextWord
       in if lastLineLen' <= maxLen
            then go lastLineLen' (nextWord : " " : wrappedTokens) wordsRest
            else go (length nextWord) (nextWord : "\n" : wrappedTokens) wordsRest
