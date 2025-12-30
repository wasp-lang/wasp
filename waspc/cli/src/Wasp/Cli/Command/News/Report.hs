{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Wasp.Cli.Command.News.Report
  ( NewsAction (..),
    makeUserInvokedNewsAction,
    executeNewsAction,
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
    markNewsAsSeen,
    saveLocalNewsState,
    setLastReportTimestamp,
    wasNewsEntrySeen,
  )
import Wasp.Cli.Interactive (askForConfirmationWithTimeout)
import Wasp.Util (ifM)
import Wasp.Util.Terminal (styleCode)

data NewsAction
  = ShowAllAndMarkSeen [NewsEntry]
  | MarkSeenWithoutShowing [NewsEntry]
  | ShowWithConfirmation [NewsEntry]
  | ShowWithoutMarkingSeen [NewsEntry]
  deriving (Show, Eq)

makeUserInvokedNewsAction :: [NewsEntry] -> NewsAction
makeUserInvokedNewsAction = ShowAllAndMarkSeen

makeWaspInvokedNewsAction :: LocalNewsState -> [NewsEntry] -> NewsAction
makeWaspInvokedNewsAction currentState allNewsEntries
  | currentState == emptyLocalNewsState = MarkSeenWithoutShowing allNewsEntries
  | hasCriticalNews = ShowWithConfirmation relevantUnseenNews
  | otherwise = ShowWithoutMarkingSeen relevantUnseenNews
  where
    hasCriticalNews = any ((== Critical) . level) relevantUnseenNews
    relevantUnseenNews = filter isRelevant . filter isUnseen $ allNewsEntries
    isRelevant = (>= Important) . level
    isUnseen = not . wasNewsEntrySeen currentState

executeNewsAction :: LocalNewsState -> NewsAction -> IO ()
executeNewsAction localState = \case
  ShowAllAndMarkSeen news -> do
    printNews news
    updateStateWithNews news
  MarkSeenWithoutShowing news ->
    updateStateWithNews news
  ShowWithConfirmation news -> do
    printNews news
    ifM askUserForConfirmation (updateStateWithNews news) updateStateOnly
  ShowWithoutMarkingSeen news -> do
    printNews news
    unless (null news) $ putStrLn $ "Run " ++ styleCode "wasp news" ++ " to mark news as seen."
    updateStateOnly
  where
    printNews news = unless (null news) $ putStrLn $ intercalate "\n\n" $ map showNewsEntry news

    askUserForConfirmation =
      askForConfirmationWithTimeout
        "\nThere are critical annoucements above. Please confirm you've read them by typing 'y'"
        "y"
        10

    updateStateWithNews news = updateLocalState news
    updateStateOnly = updateLocalState []

    updateLocalState newsToMarkAsSeen = do
      currentTime <- T.getCurrentTime
      saveLocalNewsState $
        setLastReportTimestamp currentTime $
          markNewsAsSeen newsToMarkAsSeen localState
