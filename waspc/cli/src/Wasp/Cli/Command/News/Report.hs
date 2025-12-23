{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Wasp.Cli.Command.News.Report
  ( NewsReport (..),
    makeVoluntaryNewsReport,
    makeMandatoryNewsReport,
    -- Exported only for testing purposes
    makeMandatoryNewsReportForExistingUser,
    printNewsReportAndUpdateLocalState,
  )
where

import Control.Monad (unless, when)
import Data.Maybe (isNothing)
import qualified Data.Time as T
import Wasp.Cli.Command.News.Common (NewsEntry (..), NewsLevel (..))
import Wasp.Cli.Command.News.Display (printNewsEntry)
import Wasp.Cli.Command.News.Persistence
  ( LocalNewsState (lastReportAt),
    markNewsAsSeen,
    saveLocalNewsState,
    setLastReportTimestamp,
    wasNewsEntrySeen,
  )
import Wasp.Cli.Interactive (askForInput)

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
    isFirstTimeUser = isNothing currentState.lastReportAt

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
    requireConfirmation = any ((== High) . level) allRelevantUnseenNews
    allRelevantUnseenNews = filter isRelevant . filter isUnseen $ newsEntries
    isRelevant = (>= Moderate) . level
    isUnseen = not . wasNewsEntrySeen currentState

printNewsReportAndUpdateLocalState :: LocalNewsState -> NewsReport -> IO ()
printNewsReportAndUpdateLocalState localNewsStateBeforeReport newsReport = do
  reportNews
  when newsReport.requireConfirmation askForConfirmation
  saveNewsReport
  where
    reportNews = mapM_ printNewsEntry newsReport.newsToShow

    askForConfirmation = do
      let requiredAnswer = "ok"
      answer <- askForInput $ "\nPlease type '" ++ requiredAnswer ++ "' to confirm you've read the announcements: "
      unless (answer == requiredAnswer) askForConfirmation

    saveNewsReport = do
      currentTime <- T.getCurrentTime
      saveLocalNewsState $
        setLastReportTimestamp currentTime $
          markNewsAsSeen newsReport.newsToConsiderSeen localNewsStateBeforeReport
