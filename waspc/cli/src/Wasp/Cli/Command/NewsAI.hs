{-# LANGUAGE DeriveGeneric #-}

module Wasp.Cli.Command.NewsAI
  ( news,
    checkAndDisplayNews,
  )
where

import Control.Exception (catch)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.UTF8 as ByteStringLazyUTF8
import qualified Data.ByteString.UTF8 as ByteStringUTF8
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Time as T
import qualified Data.Time.Format as TF
import GHC.Generics
import GHC.IO (unsafePerformIO)
import qualified Network.HTTP.Simple as HTTP
import StrongPath (Abs, Dir, File', Path', reldir, relfile, (</>))
import qualified StrongPath as SP
import qualified System.Directory as SD
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import qualified System.IO as SIO
import Wasp.Cli.Command (Command)
import Wasp.Cli.FileSystem (UserCacheDir, getUserCacheDir, getWaspCacheDir)
import Wasp.Util (ifM)
import qualified Wasp.Util.IO as IOUtil

-- * Public API

-- | Main command handler for explicit `wasp news` invocation.
-- Fetches all news, displays them, and marks all as seen.
news :: Command ()
news = liftIO $ do
  result <- fetchNewsFromServer
  case result of
    Left errMsg -> do
      putStrLn $ "Failed to fetch news: " ++ errMsg
      exitFailure
    Right newsItems -> do
      if null newsItems
        then putStrLn "No news available at the moment."
        else do
          displayNews newsItems
          markNewsAsSeen newsItems

-- | Background news check for `wasp start` command.
-- Fetches news if >24 hours since last fetch, filters unseen news, displays them.
checkAndDisplayNews :: IO ()
checkAndDisplayNews = do
  putStrLn "ligmaoo"
  shouldFetch <- shouldFetchNews
  when shouldFetch $ do
    putStrLn "something else"
    result <- fetchNewsFromServer
    case result of
      Left errMsg -> do
        -- Silently continue for background check, just print warning
        SIO.hPutStrLn SIO.stderr $ "Warning: Failed to fetch Wasp news: " ++ errMsg
      Right newsItems -> do
        unseenNews <- filterUnseenNews newsItems
        unless (null unseenNews) $ do
          putStrLn ""
          putStrLn "==================================="
          putStrLn "        WASP NEWS UPDATE"
          putStrLn "==================================="
          putStrLn ""
          displayNews unseenNews
          putStrLn ""
          putStrLn "Run 'wasp news' to see all news."
          putStrLn "==================================="

-- * Main command functions

-- * Display functions

-- | Display a list of news items with nice formatting.
displayNews :: [NewsItem] -> IO ()
displayNews newsItems = do
  let sortedNews = sortBy (\a b -> compare (publishedAt b) (publishedAt a)) newsItems
  mapM_ displayNewsItem sortedNews

-- | Display a single news item with formatting.
displayNewsItem :: NewsItem -> IO ()
displayNewsItem item = do
  putStrLn ""
  putStrLn $ replicate 60 '-'
  putStrLn $ levelIndicator (level item) ++ " " ++ level item ++ " - " ++ title item
  putStrLn $ replicate 60 '-'
  putStrLn ""
  putStrLn $ body item
  putStrLn ""
  putStrLn $ "Published: " ++ formatTime (publishedAt item)
  putStrLn $ "Affects: Wasp " ++ waspVersionsAffected item
  putStrLn ""
  where
    levelIndicator lvl = case lvl of
      "critical" -> "🔴"
      "warning" -> "⚠️"
      "info" -> "ℹ️"
      _ -> "📢"
    formatTime = TF.formatTime TF.defaultTimeLocale "%B %d, %Y at %H:%M UTC"

-- * Fetching functions

{-# NOINLINE waspNewsServerUrl #-}
waspNewsServerUrl :: String
waspNewsServerUrl =
  fromMaybe "https://news.wasp.sh" $ unsafePerformIO $ lookupEnv "WASP_NEWS_SERVER_URL"

-- | Fetch news from the server with error handling.
-- Returns Either error message or list of news items.
fetchNewsFromServer :: IO (Either String [NewsItem])
fetchNewsFromServer =
  ( do
      request <- HTTP.parseRequest waspNewsServerUrl
      response <- HTTP.httpBS request
      let responseBody = HTTP.getResponseBody response
      case Aeson.decode (ByteStringLazyUTF8.fromString $ ByteStringUTF8.toString responseBody) of
        Nothing -> return $ Left "Failed to parse news response"
        Just newsItems -> do
          -- Update last fetched timestamp
          updateLastFetched
          return $ Right newsItems
  )
    `catch` \(e :: HTTP.HttpException) ->
      return $ Left $ "Network error: " ++ show e

-- | Check if we should fetch news (last fetch was >24 hours ago).
shouldFetchNews :: IO Bool
shouldFetchNews = do
  maybeInfo <- readNewsInfo
  case maybeInfo of
    Nothing -> return True -- Never fetched, should fetch
    Just info -> case _lastFetched info of
      Nothing -> return True
      Just lastFetchTime -> isOlderThan24Hours lastFetchTime

-- * Cache operations

-- | Read news info from cache file.
readNewsInfo :: IO (Maybe NewsInfo)
readNewsInfo = do
  cacheFile <- getNewsCacheFilePath
  ifM
    (IOUtil.doesFileExist cacheFile)
    (Aeson.decode . ByteStringLazyUTF8.fromString . Text.unpack <$> IOUtil.readFileStrict cacheFile)
    (return Nothing)

-- | Write news info to cache file.
writeNewsInfo :: NewsInfo -> IO ()
writeNewsInfo info = do
  ensureNewsCacheDirExists
  cacheFile <- getNewsCacheFilePath
  IOUtil.writeFile cacheFile (ByteStringLazyUTF8.toString $ Aeson.encode info)

-- | Mark news items as seen by adding their IDs to the cache.
markNewsAsSeen :: [NewsItem] -> IO ()
markNewsAsSeen newsItems = do
  maybeInfo <- readNewsInfo
  let currentInfo = fromMaybe initialNewsInfo maybeInfo
  let newsIds = map getNewsId newsItems
  let updatedInfo = currentInfo {_seenNewsIds = Set.union (_seenNewsIds currentInfo) (Set.fromList newsIds)}
  writeNewsInfo updatedInfo

-- | Filter out news items that have already been seen.
filterUnseenNews :: [NewsItem] -> IO [NewsItem]
filterUnseenNews newsItems = do
  maybeInfo <- readNewsInfo
  case maybeInfo of
    Nothing -> return newsItems -- No cache, all news is unseen
    Just info -> do
      let seenIds = _seenNewsIds info
      return $ filter (\item -> not $ Set.member (getNewsId item) seenIds) newsItems

-- | Update the last fetched timestamp in the cache.
updateLastFetched :: IO ()
updateLastFetched = do
  now <- T.getCurrentTime
  maybeInfo <- readNewsInfo
  let currentInfo = fromMaybe initialNewsInfo maybeInfo
  let updatedInfo = currentInfo {_lastFetched = Just now}
  writeNewsInfo updatedInfo

-- * Helper functions

data NewsCacheDir

-- | Get the news cache directory path.
getNewsCacheDir :: Path' Abs (Dir UserCacheDir) -> Path' Abs (Dir NewsCacheDir)
getNewsCacheDir userCacheDirPath = getWaspCacheDir userCacheDirPath </> [reldir|news|]

-- | Get the news cache file path.
getNewsCacheFilePath :: IO (Path' Abs File')
getNewsCacheFilePath = do
  userCacheDir <- getUserCacheDir
  let newsCacheDir = getNewsCacheDir userCacheDir
  return $ newsCacheDir </> [relfile|cache.json|]

-- | Ensure the news cache directory exists.
ensureNewsCacheDirExists :: IO ()
ensureNewsCacheDirExists = do
  userCacheDir <- getUserCacheDir
  let newsCacheDir = getNewsCacheDir userCacheDir
  SD.createDirectoryIfMissing True $ SP.fromAbsDir newsCacheDir

-- | Get the unique ID for a news item from the API response.
getNewsId :: NewsItem -> String
getNewsId = newsId

-- | Check if a timestamp is older than 24 hours.
isOlderThan24Hours :: T.UTCTime -> IO Bool
isOlderThan24Hours time = do
  now <- T.getCurrentTime
  let secondsSinceLastFetch = T.nominalDiffTimeToSeconds (now `T.diffUTCTime` time)
  return $
    let numSecondsInHour = 3600
     in secondsSinceLastFetch > 24 * numSecondsInHour

-- * Data types

-- | A single news item from the API.
data NewsItem = NewsItem
  { newsId :: String,
    level :: String,
    title :: String,
    body :: String,
    publishedAt :: T.UTCTime,
    waspVersionsAffected :: String
  }
  deriving (Generic, Show)

instance Aeson.FromJSON NewsItem where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = modifyFieldLabel}
    where
      modifyFieldLabel "newsId" = "id"
      modifyFieldLabel other = other

instance Aeson.ToJSON NewsItem where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = modifyFieldLabel}
    where
      modifyFieldLabel "newsId" = "id"
      modifyFieldLabel other = other

-- | News cache state stored on disk.
data NewsInfo = NewsInfo
  { _lastFetched :: Maybe T.UTCTime,
    _seenNewsIds :: Set.Set String
  }
  deriving (Generic, Show)

instance Aeson.FromJSON NewsInfo

instance Aeson.ToJSON NewsInfo

-- | Initial empty news info.
initialNewsInfo :: NewsInfo
initialNewsInfo = NewsInfo {_lastFetched = Nothing, _seenNewsIds = Set.empty}
