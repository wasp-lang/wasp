module Wasp.Cli.Command.News.Fetching
  ( fetchNews,
    fetchNewsWithTimeout,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as L
import Data.Maybe (fromJust, fromMaybe)
import Data.Time (UTCTime)
import qualified Data.Time as T
import Network.HTTP.Simple (getResponseBody, httpBS, parseRequest)
import System.Environment (lookupEnv)
import Wasp.Cli.Command.News.Common (NewsEntry, debug)

fetchNewsWithTimeout :: Int -> IO (Maybe [NewsEntry])
fetchNewsWithTimeout timeoutSeconds = do
  let microsecondsInASecond = 1000000
  fetchResult <- race (threadDelay $ timeoutSeconds * microsecondsInASecond) fetchNews
  return $ case fetchResult of
    Left () -> Nothing
    Right result -> Just result

-- | TODO: Better error handling.
fetchNews :: IO [NewsEntry]
fetchNews = do
  waspNewsUrl <- fromMaybe "https://news.wasp.sh" <$> lookupEnv "WASP_NEWS_SERVER_URL"

  debug "fetching"
  response <- httpBS =<< parseRequest waspNewsUrl
  let responseBody = L.fromStrict $ getResponseBody response

  -- TODO: This fromJust here is not a good error handling, we should propagate instead.
  return $ fromJust $ decode responseBody
