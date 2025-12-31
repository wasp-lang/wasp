module Wasp.Cli.Command.News
  ( news,
    fetchAndListMustSeeNewsIfDue,
  )
where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import System.Environment (lookupEnv)
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.News.Fetching (fetchNews, fetchNewsWithTimeout)
import Wasp.Cli.Command.News.Listing
  ( NewsListing (..),
    listNews,
    shouldWaspListMustSeeNews,
  )
import Wasp.Cli.Command.News.LocalNewsState (loadLocalNewsState)
import Wasp.Util (checkIfOnCi, whenM)

news :: Command ()
news = do
  newsEntries <- either (throwError . CommandError "Getting Wasp news failed") pure =<< liftIO fetchNews
  liftIO $ do
    localNewsState <- loadLocalNewsState
    listNews localNewsState $ UserListingAllNews newsEntries

fetchAndListMustSeeNewsIfDue :: IO ()
fetchAndListMustSeeNewsIfDue = do
  isWaspNewsDisabled <- isJust <$> lookupEnv "WASP_AUTO_NEWS_DISABLE"
  isOnCi <- checkIfOnCi
  unless (isWaspNewsDisabled || isOnCi) $ do
    localNewsState <- loadLocalNewsState
    whenM (shouldWaspListMustSeeNews localNewsState) $ do
      fetchNewsWithTimeout 2 >>= \case
        -- Wasp stays silent on news fetching errors on purpose: it's not critical
        -- and we don't want to confuse the user.
        Left _err -> return ()
        Right newsEntries ->
          listNews localNewsState $ WaspListingMustSeeNews newsEntries
