{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Wasp.Cli.Command.News.Report
  ( NewsAction (..),
    makeUserInvokedNewsAction,
    executeNewsAction,
    makeWaspInvokedNewsAction,
    -- Exported only for testing purposes
    makeWaspInvokedNewsActionForExistingUser,
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

-- | A news action represents what to show and how to handle user interaction.
data NewsAction
  = -- | User ran `wasp news`: show all, mark all as seen
    ShowAllAndMarkSeen [NewsEntry]
  | -- | First-time user, wasp-initiated: mark all as seen without showing anything
    MarkSeenWithoutShowing [NewsEntry]
  | -- | Has critical news, wasp-initiated: require confirmation before marking as seen
    ShowWithConfirmation [NewsEntry]
  | -- | Important but not critical, wasp-initiated: show news without marking as seen
    ShowWithoutMarkingSeen [NewsEntry]
  deriving (Show, Eq)

-- | Create an action for when the user explicitly runs `wasp news`.
makeUserInvokedNewsAction :: [NewsEntry] -> NewsAction
makeUserInvokedNewsAction = ShowAllAndMarkSeen

-- | Create an action for wasp-initiated news display (e.g., after `wasp start`).
makeWaspInvokedNewsAction :: LocalNewsState -> [NewsEntry] -> NewsAction
makeWaspInvokedNewsAction currentState allNewsEntries
  | currentState == emptyLocalNewsState = MarkSeenWithoutShowing allNewsEntries
  | otherwise = makeWaspInvokedNewsActionForExistingUser currentState allNewsEntries

-- | Create an action for an existing user when wasp initiates the news display.
makeWaspInvokedNewsActionForExistingUser :: LocalNewsState -> [NewsEntry] -> NewsAction
makeWaspInvokedNewsActionForExistingUser currentState newsEntries
  | hasCriticalNews = ShowWithConfirmation relevantUnseenNews
  | otherwise = ShowWithoutMarkingSeen relevantUnseenNews
  where
    hasCriticalNews = any ((== Critical) . level) relevantUnseenNews
    relevantUnseenNews = filter isRelevant . filter isUnseen $ newsEntries
    isRelevant = (>= Important) . level
    isUnseen = not . wasNewsEntrySeen currentState

-- | Execute a news action: print news, handle confirmation, and update state.
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
    unless (null news) $
      putStrLn $
        "Run " ++ styleCode "wasp news" ++ " to mark news as seen."
    updateStateOnly
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

    updateStateWithNews news = updateLocalState news
    updateStateOnly = updateLocalState []

    updateLocalState newsToMarkAsSeen = do
      currentTime <- T.getCurrentTime
      saveLocalNewsState $
        setLastReportTimestamp currentTime $
          markNewsAsSeen newsToMarkAsSeen localState
