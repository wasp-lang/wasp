{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Wasp.Cli.Command.News.Listing
  ( -- * News Listing

    -- This modules captures all decisions Wasp has to make when showing Wasp
    -- news under different contexts (i.e., who requested the news listing):
    --   - Which news to show under which context.
    --   - Which news to mark as seen.
    --   - Whether the user should interactively confirm they have seen the
    --     news.
    --   - etc.
    NewsListing (..),
    getNewsToShow,
    isConfirmationRequired,
    getNewsToMarkAsSeen,
    listNews,
    shouldWaspListMustSeeNews,
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
    wasLastListingMoreThanNHoursAgo,
    wasNewsEntrySeen,
  )
import Wasp.Cli.Interactive (askForConfirmationWithTimeout)
import Wasp.Util.Terminal (styleCode)

shouldWaspListMustSeeNews :: LocalNewsState -> IO Bool
shouldWaspListMustSeeNews = wasLastListingMoreThanNHoursAgo 24

data NewsListing
  = UserListingAllNews {allNews :: [NewsEntry]}
  | WaspListingMustSeeNews {allNews :: [NewsEntry]}
  deriving (Show, Eq)

getNewsToShow :: LocalNewsState -> NewsListing -> [NewsEntry]
getNewsToShow localState = \case
  UserListingAllNews {allNews} -> allNews
  WaspListingMustSeeNews {allNews}
    | doesUserHaveNewsHistory localState -> unseenNewsThatUserMustSee allNews
    -- If the user has no news history, we don't want to hit them with all past news.
    | otherwise -> []
  where
    unseenNewsThatUserMustSee = filter ((>= Important) . level) . filter isUnseen
    isUnseen = not . wasNewsEntrySeen localState

isConfirmationRequired :: LocalNewsState -> NewsListing -> Bool
isConfirmationRequired localState listing = case listing of
  UserListingAllNews {} -> False
  WaspListingMustSeeNews {} -> any ((== Critical) . level) $ getNewsToShow localState listing

getNewsToMarkAsSeen :: LocalNewsState -> NewsListing -> [NewsEntry]
getNewsToMarkAsSeen localState listing = case listing of
  UserListingAllNews {} -> getNewsToShow localState listing
  WaspListingMustSeeNews {allNews}
    -- If the user has no news history, we want to consider all past news seen.
    | not $ doesUserHaveNewsHistory localState -> allNews
    | isConfirmationRequired localState listing -> getNewsToShow localState listing
    -- We don't want to assume the user saw the news if they haven't confirmed so
    -- explicitly.
    | otherwise -> []

listNews :: LocalNewsState -> NewsListing -> IO ()
listNews localState listing = do
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
      unless (null newsToShow) $ putStrLn $ intercalate "\n\n" $ showNewsEntry <$> newsToShow

    newsToShow = getNewsToShow localState listing

    shouldTellUserAboutWaspNewsCommand = case listing of
      UserListingAllNews {} -> False
      WaspListingMustSeeNews {} ->
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

doesUserHaveNewsHistory :: LocalNewsState -> Bool
doesUserHaveNewsHistory = (/= emptyLocalNewsState)
