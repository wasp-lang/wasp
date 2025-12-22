{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Wasp.Cli.Command.News
  ( news,
    handleNews,
    -- Exported for testing
    getMandatoryNewsReport,
    getMandatoryNewsReportForExistingUser,
    NewsReport (..),
  )
where

import Control.Monad (unless, when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust, isNothing)
import qualified Data.Time as T
import System.Environment (lookupEnv)
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.News.Common (NewsEntry (..), NewsLevel (..))
import Wasp.Cli.Command.News.Display (printNewsEntry)
import Wasp.Cli.Command.News.Fetching (fetchNews, fetchNewsWithTimeout)
import Wasp.Cli.Command.News.Persistence
  ( LocalNewsInfo (lastReportAt),
    areNewsStale,
    markNewsAsSeen,
    obtainLocalNewsInfo,
    saveLocalNewsInfo,
    setLastReportTimestamp,
    wasNewsEntrySeen,
  )
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
news =
  liftIO fetchNews >>= \case
    Left err -> throwError $ CommandError "Wasp news failed" err
    Right newsEntries ->
      liftIO $
        printNewsReportAndUpdateLocalInfo
          NewsReport
            { newsToShow = newsEntries,
              newsToConsiderSeen = newsEntries,
              requireConfirmation = False
            }

handleNews :: IO ()
handleNews = do
  isWaspNewsDisabled <- isJust <$> lookupEnv "WASP_NEWS_DISABLE"
  unless isWaspNewsDisabled $ do
    localNewsInfo <- obtainLocalNewsInfo
    whenM (areNewsStale localNewsInfo) $ do
      fetchNewsWithTimeout 2 >>= \case
        -- TODO: missing prefix for nicer output.
        Left _err -> putStrLn "Couldn't fetch Wasp news, skipping."
        Right newsEntries -> do
          let newsReport = getMandatoryNewsReport localNewsInfo newsEntries
          printNewsReportAndUpdateLocalInfo newsReport

data NewsReport = NewsReport
  { newsToShow :: [NewsEntry],
    newsToConsiderSeen :: [NewsEntry],
    requireConfirmation :: Bool
  }
  deriving (Show, Eq)

getMandatoryNewsReport :: LocalNewsInfo -> [NewsEntry] -> NewsReport
getMandatoryNewsReport localNewsInfo newsEntries
  | isFirstTimeUser = showNothingAndMarkAllAsSeen
  | otherwise = getMandatoryNewsReportForExistingUser localNewsInfo newsEntries
  where
    isFirstTimeUser = isNothing localNewsInfo.lastReportAt

    showNothingAndMarkAllAsSeen =
      NewsReport
        { newsToShow = [],
          requireConfirmation = False,
          newsToConsiderSeen = newsEntries
        }

-- TODO: better name
getMandatoryNewsReportForExistingUser :: LocalNewsInfo -> [NewsEntry] -> NewsReport
getMandatoryNewsReportForExistingUser localNewsInfo newsEntries =
  NewsReport
    { newsToShow = allRelevantUnseenNews,
      requireConfirmation,
      newsToConsiderSeen =
        if requireConfirmation
          then allRelevantUnseenNews
          else []
    }
  where
    requireConfirmation = any ((== High) . level) allRelevantUnseenNews
    allRelevantUnseenNews = filter isRelevant . filter isUnseen $ newsEntries
    isRelevant = (>= Moderate) . level
    isUnseen = not . wasNewsEntrySeen localNewsInfo

printNewsReportAndUpdateLocalInfo :: NewsReport -> IO ()
printNewsReportAndUpdateLocalInfo newsReport = do
  reportNews
  when newsReport.requireConfirmation askForConfirmation
  saveNewsReport
  where
    reportNews = do
      mapM_ printNewsEntry newsReport.newsToShow

    askForConfirmation = do
      let requiredAnswer = "ok"
      answer <- askForInput $ "\nPlease type '" ++ requiredAnswer ++ "' to confirm you've read the announcements: "
      unless (answer == requiredAnswer) askForConfirmation

    saveNewsReport = do
      -- TODO: obtaining local news twice (here and in caller), fix problem
      -- How does it even work without any lock problems?
      info <- obtainLocalNewsInfo
      currentTime <- T.getCurrentTime
      saveLocalNewsInfo $
        setLastReportTimestamp currentTime $
          markNewsAsSeen newsReport.newsToConsiderSeen info
