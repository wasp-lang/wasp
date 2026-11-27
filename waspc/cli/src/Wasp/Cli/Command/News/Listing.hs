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

import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
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
import Wasp.Cli.Console (Console (..))
import qualified Wasp.Cli.Console.Prompter as Prompter
import Wasp.Util.IO.Process (OutputStream (..))
import Wasp.Util.TextStyle (styleCode)

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

listNews :: Console -> LocalNewsState -> NewsListing -> IO ()
listNews console localState listing = do
  console.writeRawOutput Stdout . T.pack $ intercalate "\n\n" (catMaybes [newsListingStr, messageForUser]) ++ "\n"

  shouldMarkNewsAsSeen <-
    if isConfirmationRequired localState listing
      then getConfirmationFromUser
      else return True

  if shouldMarkNewsAsSeen
    then updateTimestampAndMarkAsSeen (getNewsToMarkAsSeen localState listing)
    else updateTimestampAndMarkAsSeen []
  where
    newsListingStr = showNewsListing localState listing
    messageForUser = showMessageForUser localState listing

    getConfirmationFromUser =
      case console.prompter of
        -- Without a prompter we can't ask, so we don't assume the user has
        -- seen the news.
        Nothing -> return False
        Just prompter ->
          Prompter.tryGettingConfirmationWithTimeout
            prompter
            "\nThere are critical annoucements above. Please confirm you've read them by typing 'y'"
            "y"
            10
            <&> either (const False) (const True)

    updateTimestampAndMarkAsSeen newsToMarkAsSeen = do
      currentTime <- T.getCurrentTime
      saveLocalNewsState
        $ setLastListingTimestamp currentTime
        $ markNewsAsSeen newsToMarkAsSeen localState

showMessageForUser :: LocalNewsState -> NewsListing -> Maybe String
showMessageForUser localState listing = case listing of
  UserListingAllNews {allNews}
    | null allNews -> Just "No news to report, you're all caught up!"
    | otherwise -> Nothing
  WaspListingMustSeeNews {}
    | null (getNewsToShow localState listing) -> Nothing
    | isConfirmationRequired localState listing -> Nothing
    | otherwise ->
        Just $
          "If you don't want to see these messages again, run "
            <> styleCode "wasp news"
            <> " to confirm you've seen them."

showNewsListing :: LocalNewsState -> NewsListing -> Maybe String
showNewsListing localState listing
  | null newsToShow = Nothing
  | otherwise = Just $ intercalate "\n\n" $ showNewsEntry <$> newsToShow
  where
    newsToShow = getNewsToShow localState listing

doesUserHaveNewsHistory :: LocalNewsState -> Bool
doesUserHaveNewsHistory = (/= emptyLocalNewsState)
