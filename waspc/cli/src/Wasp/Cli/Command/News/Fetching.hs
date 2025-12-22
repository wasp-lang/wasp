module Wasp.Cli.Command.News.Fetching
  ( fetchNews,
    fetchNewsWithTimeout,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Exception (try)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Network.HTTP.Simple (HttpException, parseRequest)
import System.Environment (lookupEnv)
import Wasp.Cli.Command.News.Common (NewsEntry)
import Wasp.Util.Network.HTTP (httpJSONThatThrowsIfNot2xx)

fetchNewsWithTimeout :: Int -> IO (Either String [NewsEntry])
fetchNewsWithTimeout timeoutSeconds =
  race timeout fetchNews <&> \case
    Left () -> Left "News fetching timed out"
    Right newsEntries -> newsEntries
  where
    timeout = threadDelay $ timeoutSeconds * microsecondsInASecond
    microsecondsInASecond = 1000000

fetchNews :: IO (Either String [NewsEntry])
fetchNews = do
  waspNewsUrl <- fromMaybe "https://news.wasp.sh" <$> lookupEnv "WASP_NEWS_SERVER_URL"

  requestResult <- try $ httpJSONThatThrowsIfNot2xx =<< parseRequest waspNewsUrl

  return $ case requestResult of
    -- TODO: Can I get the status code easily
    Left (_ :: HttpException) -> Left "Failed to fetch news from server."
    Right (Left jsonError) -> Left $ "Failed to decode news JSON: " ++ jsonError
    Right (Right news) -> Right news
