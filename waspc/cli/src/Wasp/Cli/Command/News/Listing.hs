{-# LANGUAGE LambdaCase #-}

module Wasp.Cli.Command.News.Listing
  ( NewsListing (..),
    getNewsToShow,
    isConfirmationRequired,
    getNewsToMarkAsSeen,
    processNewsListing,
    shouldWaspInvokeNews,
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
    wasLastLisingMoreThanNHoursAgo,
    markNewsAsSeen,
    saveLocalNewsState,
    setLastListingTimestamp,
    wasNewsEntrySeen,
  )
import Wasp.Cli.Interactive (askForConfirmationWithTimeout, waitForNSeconds)
import Wasp.Util.Terminal (styleCode)

shouldWaspInvokeNews :: LocalNewsState -> IO Bool
shouldWaspInvokeNews = wasLastLisingMoreThanNHoursAgo 24

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
    unseenNewsThatUserMustSee = filter isMustSee . filter isUnseen
    isMustSee = (>= Important) . level
    isUnseen = not . wasNewsEntrySeen localState

isConfirmationRequired :: LocalNewsState -> NewsListing -> Bool
isConfirmationRequired localState listing = case listing of
  UserRequestedAllNews _ -> False
  WaspRequestedMustSeeNews _ -> any ((== Critical) . level) (getNewsToShow localState listing)

getNewsToMarkAsSeen :: LocalNewsState -> NewsListing -> [NewsEntry]
getNewsToMarkAsSeen state listing = case listing of
  UserRequestedAllNews news -> news
  WaspRequestedMustSeeNews allNews
    | userHasNoNewsHistory state -> allNews
    | isConfirmationRequired state listing -> getNewsToShow state listing
    | otherwise -> []

processNewsListing :: LocalNewsState -> NewsListing -> IO ()
processNewsListing localState listing = do
  printNews

  when shouldTellUserAboutTheNewsCommand $ do
    putStrLn $
      "\nIf you don't want to see these messages again, run "
        ++ styleCode "wasp news"
        ++ " to confirm you read them."
    waitForNSeconds 5

  shouldMarkNewsAsSeen <-
    if isConfirmationRequired localState listing
      then askUserForConfirmation
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

    shouldTellUserAboutTheNewsCommand = case listing of
      UserRequestedAllNews _ -> False
      WaspRequestedMustSeeNews _ ->
        not (null newsToShow) && not (isConfirmationRequired localState listing)

    askUserForConfirmation =
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
