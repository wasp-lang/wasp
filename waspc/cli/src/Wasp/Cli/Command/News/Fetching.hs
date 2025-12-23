module Wasp.Cli.Command.News.Fetching
  ( fetchNewsWithTimeout,
    fetchNews,
  )
where

import Control.Exception (try)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Network.HTTP.Simple (HttpException, parseRequest)
import System.Environment (lookupEnv)
import System.Timeout (timeout)
import Wasp.Cli.Command.News.Core (NewsEntry)
import Wasp.Util.Network.HTTP (httpJSONThatThrowsIfNot2xx)

fetchNewsWithTimeout :: Int -> IO (Either String [NewsEntry])
fetchNewsWithTimeout timeoutSeconds =
  timeout timeoutMicroseconds fetchNews <&> fromMaybe (Left "News fetching timed out")
  where
    timeoutMicroseconds = timeoutSeconds * 1000000

fetchNews :: IO (Either String [NewsEntry])
fetchNews = do
  waspNewsUrl <- fromMaybe "https://news.wasp.sh" <$> lookupEnv "WASP_NEWS_SERVER_URL"

  requestResult <- try $ httpJSONThatThrowsIfNot2xx =<< parseRequest waspNewsUrl

  return $ case requestResult of
    -- TODO: Can I get the status code easily
    Left (_ :: HttpException) -> Left "Failed to fetch news from server."
    Right (Left jsonError) -> Left $ "Failed to decode news JSON: " ++ jsonError
    Right (Right news) -> Right news
