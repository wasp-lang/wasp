module Wasp.Cli.Command.News.Fetching
  ( fetchNewsWithTimeout,
    fetchNews,
  )
where

import Control.Exception (try)
import Data.Maybe (fromMaybe)
import Network.HTTP.Simple (HttpException, parseRequest)
import System.Environment (lookupEnv)
import Wasp.Cli.Command.News.Core (NewsEntry)
import Wasp.Util.Network.HTTP (httpJSONThatThrowsIfNot2xx, withTimeout)

fetchNewsWithTimeout :: Int -> IO (Either String [NewsEntry])
fetchNewsWithTimeout timeoutSeconds =
  withTimeout timeoutSeconds "News fetching timed out" fetchNews

fetchNews :: IO (Either String [NewsEntry])
fetchNews = do
  waspNewsUrl <- fromMaybe "https://news.wasp.sh" <$> lookupEnv "WASP_NEWS_SERVER_URL"

  requestResult <- try $ httpJSONThatThrowsIfNot2xx =<< parseRequest waspNewsUrl

  return $ case requestResult of
    Left (_ :: HttpException) -> Left "Failed to fetch news from server."
    Right (Left _) -> Left "Failed to decode news JSON. This is a temporary problem with the Wasp News server."
    Right (Right news) -> Right news
