{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Wasp.Cli.Command.News.Report
  ( NewsReport (..),
    makeVoluntaryNewsReport,
    isTimeForMandatoryNewsReport,
    printNewsReportAndUpdateLocalState,
    makeMandatoryNewsReport,
    -- Exported only for testing purposes
    makeMandatoryNewsReportForExistingUser,
  )
where

import Control.Monad (unless)
import Data.List (intercalate)
import qualified Data.Time as T
import Wasp.Cli.Command.News.Core (NewsEntry (..), NewsLevel (..))
import Wasp.Cli.Command.News.Display (showNewsEntry)
import Wasp.Cli.Command.News.Persistence
  ( LocalNewsState (lastReportAt),
    emptyLocalNewsState,
    markNewsAsSeen,
    saveLocalNewsState,
    setLastReportTimestamp,
    wasNewsEntrySeen,
  )
import Wasp.Cli.Interactive (askForInput)
import Wasp.Util (isOlderThanNHours)
import Wasp.Util.Terminal (styleCode)

data NewsReport = NewsReport
  { newsToShow :: [NewsEntry],
    newsToConsiderSeen :: [NewsEntry],
    requireConfirmation :: Bool
  }
  deriving (Show, Eq)

makeVoluntaryNewsReport :: LocalNewsState -> [NewsEntry] -> NewsReport
makeVoluntaryNewsReport _currentState newsEntries =
  NewsReport
    { newsToShow = newsEntries,
      newsToConsiderSeen = newsEntries,
      requireConfirmation = False
    }

makeMandatoryNewsReport :: LocalNewsState -> [NewsEntry] -> NewsReport
makeMandatoryNewsReport currentState newsEntries
  | isFirstTimeUser = showNothingAndMarkAllAsSeen
  | otherwise = makeMandatoryNewsReportForExistingUser currentState newsEntries
  where
    isFirstTimeUser = currentState == emptyLocalNewsState
    showNothingAndMarkAllAsSeen =
      NewsReport
        { newsToShow = [],
          requireConfirmation = False,
          newsToConsiderSeen = newsEntries
        }

makeMandatoryNewsReportForExistingUser :: LocalNewsState -> [NewsEntry] -> NewsReport
makeMandatoryNewsReportForExistingUser currentState newsEntries =
  NewsReport
    { newsToShow = allRelevantUnseenNews,
      requireConfirmation,
      newsToConsiderSeen =
        if requireConfirmation
          then allRelevantUnseenNews
          else []
    }
  where
    requireConfirmation = any ((== Critical) . level) allRelevantUnseenNews
    allRelevantUnseenNews = filter isRelevant . filter isUnseen $ newsEntries
    isRelevant = (>= Important) . level
    isUnseen = not . wasNewsEntrySeen currentState

isTimeForMandatoryNewsReport :: LocalNewsState -> IO Bool
isTimeForMandatoryNewsReport state = case state.lastReportAt of
  Nothing -> return True
  Just lastReportAt' -> isOlderThanNHours 24 lastReportAt'

printNewsReportAndUpdateLocalState :: LocalNewsState -> NewsReport -> IO ()
printNewsReportAndUpdateLocalState localNewsStateBeforeReport newsReport = do
  reportNews
  if newsReport.requireConfirmation
    then askForConfirmation
    else putStrLn $ "\nRun " ++ styleCode "wasp news" ++ " if you don't want to see these news again."
  updateLocalNewsState
  where
    reportNews =
      putStrLn $ intercalate "\n\n" $ map showNewsEntry newsReport.newsToShow

    askForConfirmation = do
      let requiredAnswer = "y"
      answer <-
        askForInput $
          "\nSome announcements are critical. Please confirm you've read them by typing '"
            ++ requiredAnswer
            ++ "'"
      unless (answer == requiredAnswer) askForConfirmation

    updateLocalNewsState = do
      currentTime <- T.getCurrentTime
      saveLocalNewsState $
        setLastReportTimestamp currentTime $
          markNewsAsSeen newsReport.newsToConsiderSeen localNewsStateBeforeReport
