{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Wasp.Cli.Command.News.Persistence
  ( LocalNewsInfo (..),
    obtainLocalNewsInfo,
    saveLocalNewsInfo,
    wasNewsEntrySeen,
    areNewsStale,
    setLastReportTimestamp,
    markNewsAsSeen,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.UTF8 as ByteStringLazyUTF8
import Data.Functor ((<&>))
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Time as T
import GHC.Generics
import StrongPath (Abs, File', Path', fromAbsDir, parent, relfile, (</>))
import qualified System.Directory as SD
import Wasp.Cli.Command.News.Common (NewsEntry (..))
import Wasp.Cli.FileSystem (getUserCacheDir, getWaspCacheDir)
import Wasp.Util (ifM, isOlderThanNHours)
import qualified Wasp.Util.IO as IOUtil

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

obtainLocalNewsInfo :: IO LocalNewsInfo
obtainLocalNewsInfo = do
  cacheFile <- getNewsCacheFilePath
  ifM
    (IOUtil.doesFileExist cacheFile)
    (readLocalNewsInfoFromFile cacheFile)
    (return newLocalNewsInfoFromFile)
  where
    -- TODO: perhaps better name, it might default if it fails reading
    readLocalNewsInfoFromFile filePath = do
      fileContent <- IOUtil.readFileStrict filePath
      let maybeLocalNewsInfo =
            Aeson.decode $ ByteStringLazyUTF8.fromString $ Text.unpack fileContent
      return $ fromMaybe newLocalNewsInfoFromFile maybeLocalNewsInfo

    newLocalNewsInfoFromFile =
      LocalNewsInfo
        { lastReportAt = Nothing,
          seenNewsIds = Set.empty
        }

-- | News cache state stored on disk.
data LocalNewsInfo = LocalNewsInfo
  { lastReportAt :: Maybe T.UTCTime,
    seenNewsIds :: Set String
  }
  deriving (Generic, Show, Eq)

instance Aeson.FromJSON LocalNewsInfo

instance Aeson.ToJSON LocalNewsInfo

areNewsStale :: LocalNewsInfo -> IO Bool
areNewsStale info = case info.lastReportAt of
  Nothing -> return True
  (Just lastReportAt') -> isOlderThanNHours 24 lastReportAt'

setLastReportTimestamp :: T.UTCTime -> LocalNewsInfo -> LocalNewsInfo
setLastReportTimestamp time info =
  info
    { lastReportAt = Just time
    }

markNewsAsSeen :: [NewsEntry] -> LocalNewsInfo -> LocalNewsInfo
markNewsAsSeen newsEntries info =
  info
    { seenNewsIds = seenNewsIds info `Set.union` Set.fromList ((.id) <$> newsEntries)
    }
