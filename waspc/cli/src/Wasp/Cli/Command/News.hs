{-# LANGUAGE OverloadedRecordDot #-}

module Wasp.Cli.Command.News
  ( news,
    handleNews,
  )
where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import System.Environment (lookupEnv)
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.News.Common (NewsEntry (..), debug)
import Wasp.Cli.Command.News.Display (printNewsEntry)
import Wasp.Cli.Command.News.Fetching (fetchNews, fetchNewsWithTimeout)
import Wasp.Cli.Command.News.Persistence (LocalNewsInfo (..), areNewsStale, markNewsAsSeen, obtainLocalNewsInfo, saveLocalNewsInfo, setLastFetchedTimestamp, wasNewsEntrySeen)
import Wasp.Cli.Interactive (askForInput)
import Wasp.Util (whenM)

{-
  TODO list
  - Check the TODOS from this file.
  - Handle the fetching errors (fromJust, etc.).
  - Handle the level (add filtering on wasp start and emphasize it in output, create types, etc.).
  - Properly type and validate stuff on the server.
  - Decide how to deliver the news on the server.
  - Maybe include the project in the monorepo (might make deployment more difficult).
  - Improve how the news look like in the terminal.
  - Test what happens when we add new news on the server.
  - Figure out how to end tests.
  - Figure out what to do with the versions affected field.
  - Thoroughly review the code (there are probably some hacks left over).
  - In `wasp news` output, mark the unread/new news.
-}

news :: Command ()
news = liftIO $ do
  (newsEntries, lastFetchedTimestamp) <- fetchNews

  printNewsReport $
    NewsReport
      { newsToShow = newsEntries,
        requireConfirmation = False
      }

  info <- obtainLocalNewsInfo
  saveLocalNewsInfo $
    setLastFetchedTimestamp lastFetchedTimestamp $
      markNewsAsSeen newsEntries info

handleNews :: IO ()
handleNews = do
  isWaspNewsDisabled <- isJust <$> lookupEnv "WASP_NEWS_DISABLE"
  unless isWaspNewsDisabled $ do
    localNewsInfo <- obtainLocalNewsInfo
    whenM (areNewsStale localNewsInfo) $ do
      fetchResult <- fetchNewsWithTimeout 2
      debug "Fetch timed out"
      case fetchResult of
        Nothing -> return ()
        Just (newsEntries, lastFetchedTimestamp) -> do
          let newsReport = getNewsReport localNewsInfo newsEntries
          printNewsReport newsReport
          saveLocalNewsInfo $ setLastFetchedTimestamp lastFetchedTimestamp localNewsInfo

data NewsReport = NewsReport
  { newsToShow :: [NewsEntry],
    requireConfirmation :: Bool
  }

getNewsReport :: LocalNewsInfo -> [NewsEntry] -> NewsReport
getNewsReport localNewsInfo newsEntries =
  NewsReport
    { newsToShow = relevantUnseenNews,
      requireConfirmation = thereAreCriticalNews
    }
  where
    thereAreCriticalNews = any ((== "high") . level) relevantUnseenNews
    relevantUnseenNews = filter isRelevant . filter isUnseen $ newsEntries
    isRelevant = (`elem` ["high", "moderate"]) . level
    isUnseen = not . wasNewsEntrySeen localNewsInfo

printNewsReport :: NewsReport -> IO ()
printNewsReport newsReport = do
  mapM_ printNewsEntry newsReport.newsToShow
  when newsReport.requireConfirmation askForConfirmation
  where
    askForConfirmation = do
      let requiredAnswer = "ok"
      answer <- askForInput $ "\nPlease type '" ++ requiredAnswer ++ "' to confirm you've read the announcements: "
      unless (answer == requiredAnswer) askForConfirmation
