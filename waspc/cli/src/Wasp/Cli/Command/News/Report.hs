{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Wasp.Cli.Command.News.Report
  ( NewsReport (..),
    makeVoluntaryNewsReport,
    makeMandatoryNewsReport,
    makeMandatoryNewsReportForExistingUser,
    printNewsReportAndUpdateLocalInfo,
  )
where

import Control.Monad (unless, when)
import Data.Maybe (isNothing)
import qualified Data.Time as T
import Wasp.Cli.Command.News.Common (NewsEntry (..), NewsLevel (..))
import Wasp.Cli.Command.News.Display (printNewsEntry)
import Wasp.Cli.Command.News.Persistence
  ( LocalNewsInfo (lastReportAt),
    markNewsAsSeen,
    obtainLocalNewsInfo,
    saveLocalNewsInfo,
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

makeVoluntaryNewsReport :: [NewsEntry] -> NewsReport
makeVoluntaryNewsReport newsEntries =
  NewsReport
    { newsToShow = newsEntries,
      newsToConsiderSeen = newsEntries,
      requireConfirmation = False
    }

makeMandatoryNewsReport :: LocalNewsInfo -> [NewsEntry] -> NewsReport
makeMandatoryNewsReport localNewsInfo newsEntries
  | isFirstTimeUser = showNothingAndMarkAllAsSeen
  | otherwise = makeMandatoryNewsReportForExistingUser localNewsInfo newsEntries
  where
    isFirstTimeUser = isNothing localNewsInfo.lastReportAt

    showNothingAndMarkAllAsSeen =
      NewsReport
        { newsToShow = [],
          requireConfirmation = False,
          newsToConsiderSeen = newsEntries
        }

makeMandatoryNewsReportForExistingUser :: LocalNewsInfo -> [NewsEntry] -> NewsReport
makeMandatoryNewsReportForExistingUser localNewsInfo newsEntries =
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
