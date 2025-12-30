{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Wasp.Cli.Command.News.LocalNewsState
  ( LocalNewsState,
    loadLocalNewsState,
    saveLocalNewsState,
    emptyLocalNewsState,
    isLastReportOrderThanNHours,
    wasNewsEntrySeen,
    setLastReportTimestamp,
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
import StrongPath (Abs, File', Path', fromAbsDir, parent, relfile, (</>))
import qualified System.Directory as SD
import Wasp.Cli.Command.News.Core (NewsEntry (..))
import Wasp.Cli.FileSystem (getUserCacheDir, getWaspCacheDir)
import Wasp.Util (ifM, isOlderThanNHours)
import qualified Wasp.Util.IO as IOUtil
import Wasp.Util.Json (readJsonFile, writeJsonFile)

data LocalNewsState = LocalNewsState
  { lastReportAt :: Maybe T.UTCTime,
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
  LocalNewsState {lastReportAt = Nothing, seenNewsIds = Set.empty}

wasNewsEntrySeen :: LocalNewsState -> NewsEntry -> Bool
wasNewsEntrySeen state entry = entry.id `Set.member` state.seenNewsIds

setLastReportTimestamp :: T.UTCTime -> LocalNewsState -> LocalNewsState
setLastReportTimestamp time state = state {lastReportAt = Just time}

markNewsAsSeen :: [NewsEntry] -> LocalNewsState -> LocalNewsState
markNewsAsSeen newsEntries state = state {seenNewsIds = unionOfOldAndNewIds}
  where
    unionOfOldAndNewIds = state.seenNewsIds <> Set.fromList (map (.id) newsEntries)

isLastReportOrderThanNHours :: Natural -> LocalNewsState -> IO Bool
isLastReportOrderThanNHours nHours state = case state.lastReportAt of
  Nothing -> return True
  Just lastReportAt' -> isOlderThanNHours nHours lastReportAt'

getNewsStateFilePath :: IO (Path' Abs File')
getNewsStateFilePath = do
  waspCacheDir <- getWaspCacheDir <$> getUserCacheDir
  return $ waspCacheDir </> [relfile|news.json|]
