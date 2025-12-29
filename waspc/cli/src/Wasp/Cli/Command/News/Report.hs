{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Wasp.Cli.Command.News.Report
  ( NewsReport (..),
    makeUserInvokedNewsReport,
    printNewsReportAndUpdateLocalState,
    makeWaspInvokedNewsReport,
    -- Exported only for testing purposes
    makeWaspInvokedNewsReportForExistingUser,
    NewsReportInitiator (..),
  )
where

import Control.Monad (when)
import Data.List (intercalate)
import qualified Data.Time as T
import Wasp.Cli.Command.News.Core (NewsEntry (..), NewsLevel (..))
import Wasp.Cli.Command.News.Display (showNewsEntry)
import Wasp.Cli.Command.News.LocalNewsState
  ( LocalNewsState,
    emptyLocalNewsState,
    markNewsAsSeen,
    saveLocalNewsState,
    setLastReportTimestamp,
    wasNewsEntrySeen,
  )
import Wasp.Cli.Interactive (askForConfirmationWithTimeout)
import Wasp.Util (ifM)
import Wasp.Util.Terminal (styleCode)

{-
What I want to test

- fresh news, context (who initated), state -> what to show
- news to show, context  -> ask for confirmation
- all news, confirmation, context, state -> what to consider seen

Examples:
  - Wasp initiates, state is empty -> mark all as seen, show nothing
  - Wasp initiates, state exists -> show relevant news
    - relevant contain critical -> requie confirmation
      - user confirmed -> mark as seen
    - relevant don't contain cricical -> don't require confirmation, don't mark as seen
  - User initiates -> show everything, mark everything as seen
-}

data NewsReportInitiator = Wasp | User deriving (Show, Eq)

data NewsReport = NewsReport
  { newsToShow :: [NewsEntry],
    initiator :: NewsReportInitiator,
    newsToConsiderSeen :: [NewsEntry],
    requireConfirmation :: Bool
  }
  deriving (Show, Eq)

makeUserInvokedNewsReport :: LocalNewsState -> [NewsEntry] -> NewsReport
makeUserInvokedNewsReport _currentState newsEntries =
  NewsReport
    { initiator = User,
      newsToShow = newsEntries,
      newsToConsiderSeen = newsEntries,
      requireConfirmation = False
    }

makeWaspInvokedNewsReport :: LocalNewsState -> [NewsEntry] -> NewsReport
makeWaspInvokedNewsReport currentState newsEntries
  | isFirstTimeUser = showNothingAndMarkAllAsSeen
  | otherwise = makeWaspInvokedNewsReportForExistingUser currentState newsEntries
  where
    isFirstTimeUser = currentState == emptyLocalNewsState
    showNothingAndMarkAllAsSeen =
      NewsReport
        { initiator = Wasp,
          newsToShow = [],
          requireConfirmation = False,
          newsToConsiderSeen = newsEntries
        }

makeWaspInvokedNewsReportForExistingUser :: LocalNewsState -> [NewsEntry] -> NewsReport
makeWaspInvokedNewsReportForExistingUser currentState newsEntries =
  NewsReport
    { initiator = Wasp,
      newsToShow = allRelevantUnseenNews,
      requireConfirmation,
      newsToConsiderSeen = allRelevantUnseenNews
    }
  where
    requireConfirmation = any ((== Critical) . level) allRelevantUnseenNews
    allRelevantUnseenNews = filter isRelevant . filter isUnseen $ newsEntries
    isRelevant = (>= Important) . level
    isUnseen = not . wasNewsEntrySeen currentState

-- newsToConsiderSeen -> newsToMarkAsSeen
-- requireConfirmation -> requiresConfirmation

showNewsReport :: NewsReport -> String
showNewsReport newsReport = intercalate "\n\n" $ map showNewsEntry newsReport.newsToShow

printNewsReportAndUpdateLocalState :: LocalNewsState -> NewsReport -> IO ()
printNewsReportAndUpdateLocalState localNewsStateBeforeReport newsReport = case newsReport of
  NewsReport {initiator = Wasp} -> printWaspInitiatedReport
  NewsReport {initiator = User} -> printUserInitiatedReport
  where
    printWaspInitiatedReport = do
      when thereAreNewsToShow $ putStrLn $ showNewsReport newsReport
      if newsReport.requireConfirmation
        then
          ifM
            askUserForConfirmation
            updateTimestampAndMarkNewsAsSeen
            updateTimestampWithoutMarkingNewsAsSeen
        else do
          when thereAreNewsToShow $ putStrLn $ "Run " ++ styleCode "wasp news" ++ " to mark news as seen."
          updateTimestampWithoutMarkingNewsAsSeen

    printUserInitiatedReport = do
      when thereAreNewsToShow $ putStrLn $ showNewsReport newsReport
      updateTimestampAndMarkNewsAsSeen

    updateTimestampAndMarkNewsAsSeen = updateLocalNewsState newsReport.newsToConsiderSeen
    updateTimestampWithoutMarkingNewsAsSeen = updateLocalNewsState []

    askUserForConfirmation =
      askForConfirmationWithTimeout
        "\nThere are critical annoucements above. Please confirm you've read them by typing 'y'"
        "y"
        10

    thereAreNewsToShow = not $ null newsReport.newsToShow

    updateLocalNewsState newsToMarkAsSeen = do
      currentTime <- T.getCurrentTime
      saveLocalNewsState $
        setLastReportTimestamp currentTime $
          markNewsAsSeen newsToMarkAsSeen localNewsStateBeforeReport
