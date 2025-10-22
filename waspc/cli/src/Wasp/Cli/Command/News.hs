{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.Cli.Command.News
  ( news,
    ifNewsStaleUpdateAndShowUnseen,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (parseJSON), decode, genericParseJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as ByteStringLazyUTF8
import Data.Functor ((<&>))
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import qualified Data.Time as T
import GHC.Generics
import GHC.IO (unsafePerformIO)
import Network.HTTP.Simple (getResponseBody, httpBS, parseRequest)
import StrongPath (Abs, File', Path', fromAbsDir, parent, relfile, (</>))
import qualified System.Directory as SD
import System.Environment (lookupEnv)
import Wasp.Cli.Command (Command)
import Wasp.Cli.FileSystem (getUserCacheDir, getWaspCacheDir)
import Wasp.Util (ifM, indent, whenM)
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
-}

news :: Command ()
news = liftIO $ do
  newsEntries <- fetchNews

  printNews newsEntries

  info <- obtainLocalNewsInfo
  currentTime <- T.getCurrentTime
  saveLocalNewsInfo $
    info
      { _lastFetched = Just currentTime,
        _seenNewsIds =
          _seenNewsIds info
            `Set.union` Set.fromList (_wneId <$> newsEntries)
      }

ifNewsStaleUpdateAndShowUnseen :: IO ()
ifNewsStaleUpdateAndShowUnseen = do
  localNewsInfo <- obtainLocalNewsInfo
  whenM (areNewsStale localNewsInfo) $ do
    unseenNews <- filter (not . wasNewsEntrySeen localNewsInfo) <$> fetchNews
    printNews unseenNews
    currentTime <- T.getCurrentTime
    saveLocalNewsInfo $ localNewsInfo {_lastFetched = Just currentTime}

{-# NOINLINE waspNewsServerUrl #-}
waspNewsServerUrl :: String
waspNewsServerUrl =
  fromMaybe "https://news.wasp.sh" $ unsafePerformIO $ lookupEnv "WASP_NEWS_SERVER_URL"

data NewsEntry = NewsEntry
  { _wneId :: !String,
    _wneTitle :: !String,
    _wneBody :: !String,
    _wneLevel :: !String,
    _wnePublishedAt :: !UTCTime
  }
  deriving (Generic, Show)

instance FromJSON NewsEntry where
  parseJSON =
    genericParseJSON $
      Aeson.defaultOptions {Aeson.fieldLabelModifier = modifyFieldLabel}
    where
      modifyFieldLabel "_wneId" = "id"
      modifyFieldLabel "_wneLevel" = "level"
      modifyFieldLabel "_wneTitle" = "title"
      modifyFieldLabel "_wneBody" = "body"
      modifyFieldLabel "_wnePublishedAt" = "publishedAt"
      modifyFieldLabel other = other

printNewsEntry :: NewsEntry -> IO ()
printNewsEntry entry = do
  putStrLn ""
  putStrLn $
    Term.applyStyles [Term.Bold] title
      <> " "
      <> Term.applyStyles [Term.Bold] (replicate dotCount '.')
      <> " "
      <> Term.applyStyles [Term.Yellow, Term.Bold] dateText
  putStrLn $
    levelPrint
      <> "\n"
      <> Term.applyStyles [Term.Grey] (indent 2 $ wrapText (maxColumns - 2) body)
  where
    title = _wneTitle entry
    level = _wneLevel entry
    body = _wneBody entry
    dateText = formatTime defaultTimeLocale "%Y-%m-%d" (_wnePublishedAt entry)
    dotCount = max minDotsCount (maxColumns - length title - length dateText - 2)
    maxColumns = 80
    minDotsCount = 5
    levelPrint =
      "Severity: "
        ++ ( case level of
               "high" -> Term.applyStyles [Term.Red]
               "moderate" -> Term.applyStyles [Term.Yellow]
               "low" -> Term.applyStyles [Term.Blue]
               _ -> error "Invalid"
           )
          level

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

printNews :: [NewsEntry] -> IO ()
printNews newsEntries = do
  -- waspSays $ asWaspStartMessage "Here's some news for ya!"
  mapM_ printNewsEntry newsEntries

wasNewsEntrySeen :: LocalNewsInfo -> NewsEntry -> Bool
wasNewsEntrySeen info entry = _wneId entry `Set.member` _seenNewsIds info

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
      return $ fromJust $ Aeson.decode $ ByteStringLazyUTF8.fromString $ Text.unpack fileContent
    newLocalNewsInfoFromFile =
      LocalNewsInfo
        { _lastFetched = Nothing,
          _seenNewsIds = Set.empty
        }

-- | News cache state stored on disk.
data LocalNewsInfo = LocalNewsInfo
  { _lastFetched :: Maybe T.UTCTime,
    _seenNewsIds :: Set String
  }
  deriving (Generic, Show)

instance Aeson.FromJSON LocalNewsInfo

instance Aeson.ToJSON LocalNewsInfo

areNewsStale :: LocalNewsInfo -> IO Bool
areNewsStale info = case _lastFetched info of
  Nothing -> return True
  (Just lastFetched) -> isOlderThan24Hours lastFetched

-- | TODO: Better error handling.
fetchNews :: IO [NewsEntry]
fetchNews = do
  response <- httpBS =<< parseRequest waspNewsServerUrl
  let responseBody = L.fromStrict $ getResponseBody response
  -- TODO: This fromJust here is not a good error handling, we should propagate instead.
  return $ fromJust $ decode responseBody

-- | TODO: Instead reuse similar function from telemetry (it is 12 hours though).
isOlderThan24Hours :: T.UTCTime -> IO Bool
isOlderThan24Hours time = do
  now <- T.getCurrentTime
  let secondsSinceLastFetch = T.nominalDiffTimeToSeconds (now `T.diffUTCTime` time)
  return $
    let numSecondsInHour = 3600
     in secondsSinceLastFetch > 24 * numSecondsInHour
