{-# LANGUAGE LambdaCase #-}

module Wasp.Cli.Command.News.Listing
  ( -- * News Listing

    -- This modules captures all decisions Wasp has to make when showing Wasp
    -- news:
    --   - Which news to show under which context (i.e., who requested it).
    --   - Which news to mark as seen.
    --   - Whether the user should interactively confirm they've read the news.
    --   - etc.
    --
    -- We tried to caputure most of the logic in pure function to make it
    -- simplify testing.
    NewsListing (..),
    getNewsToShow,
    isConfirmationRequired,
    getNewsToMarkAsSeen,
    processNewsListing,
    shouldWaspListMustSeeNews,
    getNewsFromListing,
  )
where

import Control.Monad (unless, when)
import Data.List (intercalate)
import qualified Data.Time as T
import Wasp.Cli.Command.News.Core (NewsEntry (..), NewsLevel (..))
import Wasp.Cli.Command.News.Display (showNewsEntry)
import Wasp.Cli.Command.News.LocalNewsState
  ( LocalNewsState,
    emptyLocalNewsState,
    markNewsAsSeen,
    saveLocalNewsState,
    setLastListingTimestamp,
    wasLastLisingMoreThanNHoursAgo,
    wasNewsEntrySeen,
  )
import Wasp.Cli.Interactive (askForConfirmationWithTimeout)
import Wasp.Util.Terminal (styleCode)

shouldWaspListMustSeeNews :: LocalNewsState -> IO Bool
shouldWaspListMustSeeNews = wasLastLisingMoreThanNHoursAgo 24

data NewsListing
  = UserRequestedAllNews [NewsEntry]
  | WaspRequestedMustSeeNews [NewsEntry]
  deriving (Show, Eq)

getNewsToShow :: LocalNewsState -> NewsListing -> [NewsEntry]
getNewsToShow localState = \case
  UserRequestedAllNews allNews -> allNews
  WaspRequestedMustSeeNews allNews
    | userHasNoNewsHistory localState -> []
    | otherwise -> unseenNewsThatUserMustSee allNews
  where
    unseenNewsThatUserMustSee = filter ((>= Important) . level) . filter isUnseen
    isUnseen = not . wasNewsEntrySeen localState

isConfirmationRequired :: LocalNewsState -> NewsListing -> Bool
isConfirmationRequired localState listing = case listing of
  UserRequestedAllNews _ -> False
  WaspRequestedMustSeeNews _ -> any ((== Critical) . level) $ getNewsToShow localState listing

getNewsToMarkAsSeen :: LocalNewsState -> NewsListing -> [NewsEntry]
getNewsToMarkAsSeen localState listing = case listing of
  UserRequestedAllNews allNews -> allNews
  WaspRequestedMustSeeNews allNews
    | userHasNoNewsHistory localState -> allNews
    | isConfirmationRequired localState listing -> getNewsToShow localState listing
    | otherwise -> []

processNewsListing :: LocalNewsState -> NewsListing -> IO ()
processNewsListing localState listing = do
  printNews

  when shouldTellUserAboutWaspNewsCommand $ do
    putStrLn $
      "\nIf you don't want to see these messages again, run "
        ++ styleCode "wasp news"
        ++ " to confirm you've seen them."

  shouldMarkNewsAsSeen <-
    if isConfirmationRequired localState listing
      then getConfirmationFromUser
      else return True

  if shouldMarkNewsAsSeen
    then updateTimestampAndMarkAsSeen (getNewsToMarkAsSeen localState listing)
    else updateTimestampAndMarkAsSeen []
  where
    printNews =
      unless (null newsToShow) $
        putStrLn $
          intercalate "\n\n" $
            map showNewsEntry newsToShow

    newsToShow = getNewsToShow localState listing

    shouldTellUserAboutWaspNewsCommand = case listing of
      UserRequestedAllNews _ -> False
      WaspRequestedMustSeeNews _ ->
        not (null newsToShow) && not (isConfirmationRequired localState listing)

    getConfirmationFromUser =
      askForConfirmationWithTimeout
        "\nThere are critical annoucements above. Please confirm you've read them by typing 'y'"
        "y"
        10

    updateTimestampAndMarkAsSeen newsToMarkAsSeen = do
      currentTime <- T.getCurrentTime
      saveLocalNewsState $
        setLastListingTimestamp currentTime $
          markNewsAsSeen newsToMarkAsSeen localState

userHasNoNewsHistory :: LocalNewsState -> Bool
userHasNoNewsHistory = (== emptyLocalNewsState)

getNewsFromListing :: NewsListing -> [NewsEntry]
getNewsFromListing = \case
  UserRequestedAllNews allNews -> allNews
  WaspRequestedMustSeeNews allNews -> allNews
