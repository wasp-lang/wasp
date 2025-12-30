{-# LANGUAGE LambdaCase #-}

module Wasp.Cli.Command.News.Action
  ( NewsAction (..),
    makeUserInvokedNewsAction,
    executeNewsAction,
    shouldWaspInvokeNews,
    makeWaspInvokedNewsAction,
  )
where

import Control.Monad (unless)
import Data.List (intercalate)
import qualified Data.Time as T
import Wasp.Cli.Command.News.Core (NewsEntry (..), NewsLevel (..))
import Wasp.Cli.Command.News.Display (showNewsEntry)
import Wasp.Cli.Command.News.LocalNewsState
  ( LocalNewsState,
    emptyLocalNewsState,
    isLastReportOrderThanNHours,
    markNewsAsSeen,
    saveLocalNewsState,
    setLastReportTimestamp,
    wasNewsEntrySeen,
  )
import Wasp.Cli.Interactive (askForConfirmationWithTimeout)
import Wasp.Util (ifM)
import Wasp.Util.Terminal (styleCode)

data NewsAction
  = ShowAllAndMarkSeen [NewsEntry] -- wasp news
  | MarkSeenWithoutShowing [NewsEntry] -- wasp start + no previous state
  | ShowWithConfirmation [NewsEntry] -- wasp start + there are critical news
  | ShowWithoutMarkingSeen [NewsEntry] -- wasp start + no critical news
  deriving (Show, Eq)

shouldWaspInvokeNews :: LocalNewsState -> IO Bool
shouldWaspInvokeNews = isLastReportOrderThanNHours 24

-- Try martin's idea with two sum types.

makeUserInvokedNewsAction :: [NewsEntry] -> NewsAction
makeUserInvokedNewsAction = ShowAllAndMarkSeen

makeWaspInvokedNewsAction :: LocalNewsState -> [NewsEntry] -> NewsAction
makeWaspInvokedNewsAction currentState allNewsEntries
  | userHasNoNewsHistory = MarkSeenWithoutShowing allNewsEntries
  | thereAreCriticalNews = ShowWithConfirmation relevantUnseenNews
  | otherwise = ShowWithoutMarkingSeen relevantUnseenNews
  where
    userHasNoNewsHistory = currentState == emptyLocalNewsState
    thereAreCriticalNews = any ((== Critical) . level) relevantUnseenNews
    relevantUnseenNews = filter isRelevant . filter isUnseen $ allNewsEntries
    isRelevant = (>= Important) . level
    isUnseen = not . wasNewsEntrySeen currentState

executeNewsAction :: LocalNewsState -> NewsAction -> IO ()
executeNewsAction localState = \case
  ShowAllAndMarkSeen news -> do
    printNews news
    updateTimestampAndMarkAsSeen news
  MarkSeenWithoutShowing news ->
    updateTimestampAndMarkAsSeen news
  ShowWithoutMarkingSeen news -> do
    printNews news
    unless (null news) $ putStrLn $ "Run " ++ styleCode "wasp news" ++ " to mark news as seen."
    updateTimestampWithoutMarkingNewsAsSeen
  ShowWithConfirmation news -> do
    printNews news
    ifM
      askUserForConfirmation
      (updateTimestampAndMarkAsSeen news)
      updateTimestampWithoutMarkingNewsAsSeen
  where
    printNews news =
      unless (null news) $
        putStrLn $
          intercalate "\n\n" $
            map showNewsEntry news

    askUserForConfirmation =
      askForConfirmationWithTimeout
        "\nThere are critical annoucements above. Please confirm you've read them by typing 'y'"
        "y"
        10

    updateTimestampWithoutMarkingNewsAsSeen = updateTimestampAndMarkAsSeen []

    updateTimestampAndMarkAsSeen newsToMarkAsSeen = do
      currentTime <- T.getCurrentTime
      saveLocalNewsState $
        setLastReportTimestamp currentTime $
          markNewsAsSeen newsToMarkAsSeen localState
