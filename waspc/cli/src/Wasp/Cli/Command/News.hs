{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Wasp.Cli.Command.News
  ( news,
    handleNews,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, decode)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as ByteStringLazyUTF8
import Data.Functor ((<&>))
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import qualified Data.Time as T
import GHC.Generics
import Network.HTTP.Simple (getResponseBody, httpBS, parseRequest)
import StrongPath (Abs, File', Path', fromAbsDir, parent, relfile, (</>))
import qualified System.Directory as SD
import System.Environment (lookupEnv)
import Wasp.Cli.Command (Command)
import Wasp.Cli.FileSystem (getUserCacheDir, getWaspCacheDir)
import Wasp.Cli.Interactive (askForInput)
import Wasp.Util (ifM, indent, isOlderThanNHours, trim, whenM)
import qualified Wasp.Util.IO as IOUtil
import qualified Wasp.Util.Terminal as Term

{-
  TODO list
  - Check the TODOS from this file.
  - Handle the fetching errors (fromJust, etc.).
  - Handle the level (add filtering on wasp start and emphasize it in output, create types, etc.).
  - Properly type and validate stuff on the server.
  - Decide how to deliver the news on the server.
  - Maybe include the project in the monorepo (might make deployment more difficult).
  - Improve how the news look like in the terminal .
  - Improve naming and considerj splitting into multiple files.
  - Test what happens when we add new news on the server.
  - Figure out how to end tests.
  - Figure out what to do with the versions affected field.
  - Thoroughly review the code (there are probably some hacks left over).
  - In `wasp news` output, mark the unread/new news.
-}

news :: Command ()
news = liftIO $ do
  (newsEntries, lastFetchedTimestamp) <- fetchNews

  printNewsReport $
    NewsReport
      { newsToShow = newsEntries,
        requireConfirmation = False
      }

  info <- obtainLocalNewsInfo
  saveLocalNewsInfo $
    setLastFetchedTimestamp lastFetchedTimestamp $
      markNewsAsSeen newsEntries info

setLastFetchedTimestamp :: UTCTime -> LocalNewsInfo -> LocalNewsInfo
setLastFetchedTimestamp time info =
  info
    { lastFetched = Just time
    }

markNewsAsSeen :: [NewsEntry] -> LocalNewsInfo -> LocalNewsInfo
markNewsAsSeen newsEntries info =
  info
    { seenNewsIds = seenNewsIds info `Set.union` Set.fromList ((.id) <$> newsEntries)
    }

handleNews :: IO ()
handleNews = do
  isWaspNewsDisabled <- isJust <$> lookupEnv "WASP_NEWS_DISABLE"
  unless isWaspNewsDisabled $ do
    localNewsInfo <- obtainLocalNewsInfo
    whenM (areNewsStale localNewsInfo) $ do
      fetchResult <- fetchNewsWithTimeout 2
      debug "Fetch timed out"
      case fetchResult of
        Nothing -> return ()
        Just (newsEntries, lastFetchedTimestamp) -> do
          let newsReport = getNewsReport localNewsInfo newsEntries
          printNewsReport newsReport
          saveLocalNewsInfo $ setLastFetchedTimestamp lastFetchedTimestamp localNewsInfo

data NewsReport = NewsReport
  { newsToShow :: [NewsEntry],
    requireConfirmation :: Bool
  }

getNewsReport :: LocalNewsInfo -> [NewsEntry] -> NewsReport
getNewsReport localNewsInfo newsEntries =
  NewsReport
    { newsToShow = relevantUnseenNews,
      requireConfirmation = thereAreCriticalNews
    }
  where
    thereAreCriticalNews = any ((== "high") . level) relevantUnseenNews
    relevantUnseenNews = filter isRelevant . filter isUnseen $ newsEntries
    isRelevant = (`elem` ["high", "moderate"]) . level
    isUnseen = not . wasNewsEntrySeen localNewsInfo

printNewsReport :: NewsReport -> IO ()
printNewsReport newsReport = do
  mapM_ printNewsEntry newsReport.newsToShow
  when newsReport.requireConfirmation askForConfirmation
  where
    askForConfirmation = do
      let requiredAnswer = "ok"
      answer <- askForInput $ "\nPlease type '" ++ requiredAnswer ++ "' to confirm you've read the announcements: "
      unless (answer == requiredAnswer) askForConfirmation

fetchNewsWithTimeout :: Int -> IO (Maybe ([NewsEntry], UTCTime))
fetchNewsWithTimeout timeoutSeconds = do
  let microsecondsInASecond = 1000000
  fetchResult <- race (threadDelay $ timeoutSeconds * microsecondsInASecond) fetchNews
  return $ case fetchResult of
    Left () -> Nothing
    Right result -> Just result

debug :: String -> IO ()
debug message = print message

data NewsEntry = NewsEntry
  { id :: !String,
    title :: !String,
    body :: !String,
    level :: !String,
    publishedAt :: !UTCTime
  }
  deriving (Generic, Show)

instance FromJSON NewsEntry

printNewsEntry :: NewsEntry -> IO ()
printNewsEntry entry = do
  putStrLn ""
  putStrLn $
    Term.applyStyles [Term.Bold] entry.title
      <> " "
      <> Term.applyStyles [Term.Bold] (replicate dotCount '.')
      <> " "
      <> Term.applyStyles [Term.Yellow, Term.Bold] dateText
  putStrLn $
    showLevelInColor entry.level
      <> "\n"
      <> Term.applyStyles [Term.Grey] (indent 2 $ wrapText (maxColumns - 2) entry.body)
  where
    dateText = formatTime defaultTimeLocale "%Y-%m-%d" (publishedAt entry)
    dotCount = max minDotsCount (maxColumns - length entry.title - length dateText - 2)
    maxColumns = 80
    minDotsCount = 5

showLevelInColor :: String -> String
showLevelInColor newsLevel = styleLevel newsLevel
  where
    styleLevel = case newsLevel of
      "high" -> Term.applyStyles [Term.Red]
      "moderate" -> Term.applyStyles [Term.Yellow]
      "low" -> Term.applyStyles [Term.Blue]
      _ -> error "Invalid"

wrapText :: Int -> String -> String
wrapText maxLen text = go 0 [] (words text)
  where
    go :: Int -> [String] -> [String] -> String
    go _ wrappedTokens [] = concat $ drop 1 $ reverse wrappedTokens
    go lastLineLen wrappedTokens (nextWord : wordsRest) =
      let lastLineLen' = lastLineLen + 1 + length nextWord
       in if lastLineLen' <= maxLen
            then go lastLineLen' (nextWord : " " : wrappedTokens) wordsRest
            else go (length nextWord) (nextWord : "\n" : wrappedTokens) wordsRest

getNewsCacheFilePath :: IO (Path' Abs File')
getNewsCacheFilePath = getUserCacheDir <&> (</> [relfile|news.json|]) . getWaspCacheDir

ensureNewsCacheFileParentDirExists :: IO ()
ensureNewsCacheFileParentDirExists = do
  parentDir <- parent <$> getNewsCacheFilePath
  SD.createDirectoryIfMissing True $ fromAbsDir parentDir

saveLocalNewsInfo :: LocalNewsInfo -> IO ()
saveLocalNewsInfo localNewsInfo = do
  ensureNewsCacheFileParentDirExists
  newsCacheFile <- getNewsCacheFilePath
  IOUtil.writeFile newsCacheFile $ ByteStringLazyUTF8.toString $ Aeson.encode localNewsInfo

wasNewsEntrySeen :: LocalNewsInfo -> NewsEntry -> Bool
wasNewsEntrySeen info entry = entry.id `Set.member` seenNewsIds info

-- | TODO: Improve error handling.
obtainLocalNewsInfo :: IO LocalNewsInfo
obtainLocalNewsInfo = do
  cacheFile <- getNewsCacheFilePath
  ifM
    (IOUtil.doesFileExist cacheFile)
    (readLocalNewsInfoFromFile cacheFile)
    (return newLocalNewsInfoFromFile)
  where
    readLocalNewsInfoFromFile filePath = do
      fileContent <- IOUtil.readFileStrict filePath
      -- TODO: figure out what to do if this file is invalid
      return $ fromJust $ Aeson.decode $ ByteStringLazyUTF8.fromString $ Text.unpack fileContent
    newLocalNewsInfoFromFile =
      LocalNewsInfo
        { lastFetched = Nothing,
          seenNewsIds = Set.empty
        }

-- | News cache state stored on disk.
data LocalNewsInfo = LocalNewsInfo
  { lastFetched :: Maybe T.UTCTime,
    seenNewsIds :: Set String
  }
  deriving (Generic, Show)

instance Aeson.FromJSON LocalNewsInfo

instance Aeson.ToJSON LocalNewsInfo

areNewsStale :: LocalNewsInfo -> IO Bool
areNewsStale info = case info.lastFetched of
  Nothing -> return True
  (Just lastFetched') -> isOlderThanNHours 24 lastFetched'

-- | TODO: Better error handling.
fetchNews :: IO ([NewsEntry], UTCTime)
fetchNews = do
  waspNewsUrl <- fromMaybe "https://news.wasp.sh" <$> lookupEnv "WASP_NEWS_SERVER_URL"

  debug "fetching"
  response <- httpBS =<< parseRequest waspNewsUrl
  let responseBody = L.fromStrict $ getResponseBody response

  currentTime <- T.getCurrentTime
  -- TODO: This fromJust here is not a good error handling, we should propagate instead.
  return (fromJust $ decode responseBody, currentTime)
