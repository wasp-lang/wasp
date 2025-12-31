{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Wasp.Cli.Command.News.LocalNewsState
  ( LocalNewsState,
    loadLocalNewsState,
    saveLocalNewsState,
    emptyLocalNewsState,
    wasLastLisingMoreThanNHoursAgo,
    wasNewsEntrySeen,
    setLastListingTimestamp,
    markNewsAsSeen,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Time as T
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import StrongPath
  ( Abs,
    File,
    Path',
    Rel,
    fromAbsDir,
    parent,
    relfile,
    (</>),
  )
import qualified System.Directory as SD
import Wasp.Cli.Command.News.Core (NewsEntry (..))
import Wasp.Cli.FileSystem (WaspCacheDir, getUserCacheDir, getWaspCacheDir)
import Wasp.Util (ifM, isOlderThanNHours)
import qualified Wasp.Util.IO as IOUtil
import Wasp.Util.Json (readJsonFile, writeJsonFile)

data LocalNewsState = LocalNewsState
  { lastListingAt :: Maybe T.UTCTime,
    seenNewsIds :: Set String
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

saveLocalNewsState :: LocalNewsState -> IO ()
saveLocalNewsState localNewsState = do
  newsStateFile <- getNewsStateFilePath
  SD.createDirectoryIfMissing True $ fromAbsDir $ parent newsStateFile
  writeJsonFile newsStateFile localNewsState

loadLocalNewsState :: IO LocalNewsState
loadLocalNewsState = do
  stateFile <- getNewsStateFilePath
  ifM
    (IOUtil.doesFileExist stateFile)
    (tryReadingLocalNewsStateOrUseEmpty stateFile)
    (return emptyLocalNewsState)
  where
    tryReadingLocalNewsStateOrUseEmpty stateFile =
      fromMaybe emptyLocalNewsState <$> readJsonFile stateFile

emptyLocalNewsState :: LocalNewsState
emptyLocalNewsState =
  LocalNewsState {lastListingAt = Nothing, seenNewsIds = Set.empty}

wasNewsEntrySeen :: LocalNewsState -> NewsEntry -> Bool
wasNewsEntrySeen state entry = entry.id `Set.member` state.seenNewsIds

setLastListingTimestamp :: T.UTCTime -> LocalNewsState -> LocalNewsState
setLastListingTimestamp time state = state {lastListingAt = Just time}

markNewsAsSeen :: [NewsEntry] -> LocalNewsState -> LocalNewsState
markNewsAsSeen newsToMark state = state {seenNewsIds = oldSeenIds <> newSeenIds}
  where
    oldSeenIds = state.seenNewsIds
    newSeenIds = Set.fromList $ map (.id) newsToMark

wasLastLisingMoreThanNHoursAgo :: Natural -> LocalNewsState -> IO Bool
wasLastLisingMoreThanNHoursAgo nHours state = case state.lastListingAt of
  Nothing -> return True
  Just lastListingAt' -> isOlderThanNHours nHours lastListingAt'

getNewsStateFilePath :: IO (Path' Abs (File LocalNewsStateFile))
getNewsStateFilePath = do
  waspCacheDir <- getWaspCacheDir <$> getUserCacheDir
  return $ waspCacheDir </> newsStateFileInUserCacheDir

data LocalNewsStateFile

newsStateFileInUserCacheDir :: Path' (Rel WaspCacheDir) (File LocalNewsStateFile)
newsStateFileInUserCacheDir = [relfile|news.json|]
