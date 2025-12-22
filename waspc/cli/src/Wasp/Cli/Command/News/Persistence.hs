{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Wasp.Cli.Command.News.Persistence
  ( LocalNewsState (..),
    obtainLocalNewsState,
    saveLocalNewsState,
    wasNewsEntrySeen,
    setLastReportTimestamp,
    markNewsAsSeen,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.UTF8 as ByteStringLazyUTF8
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Time as T
import GHC.Generics
import StrongPath (Abs, File', Path', fromAbsDir, parent, relfile, (</>))
import qualified System.Directory as SD
import Wasp.Cli.Command.News.Common (NewsEntry (..))
import Wasp.Cli.FileSystem (getUserCacheDir, getWaspCacheDir)
import Wasp.Util (ifM)
import qualified Wasp.Util.IO as IOUtil

-- | News state stored on disk.
data LocalNewsState = LocalNewsState
  { lastReportAt :: Maybe T.UTCTime,
    seenNewsIds :: Set String
  }
  deriving (Generic, Show, Eq)

instance Aeson.FromJSON LocalNewsState

instance Aeson.ToJSON LocalNewsState

saveLocalNewsState :: LocalNewsState -> IO ()
saveLocalNewsState localNewsState = do
  ensureNewsStateFileParentDirExists
  newsStateFile <- getNewsStateFilePath
  IOUtil.writeFile newsStateFile $ ByteStringLazyUTF8.toString $ Aeson.encode localNewsState

obtainLocalNewsState :: IO LocalNewsState
obtainLocalNewsState = do
  stateFile <- getNewsStateFilePath
  ifM
    (IOUtil.doesFileExist stateFile)
    (readLocalNewsStateFromFile stateFile)
    (return emptyLocalNewsState)
  where
    readLocalNewsStateFromFile filePath = do
      fileContent <- IOUtil.readFileStrict filePath
      let maybeLocalNewsState =
            Aeson.decode $ ByteStringLazyUTF8.fromString $ Text.unpack fileContent
      return $ fromMaybe emptyLocalNewsState maybeLocalNewsState

    emptyLocalNewsState =
      LocalNewsState
        { lastReportAt = Nothing,
          seenNewsIds = Set.empty
        }

wasNewsEntrySeen :: LocalNewsState -> NewsEntry -> Bool
wasNewsEntrySeen state entry = entry.id `Set.member` seenNewsIds state

setLastReportTimestamp :: T.UTCTime -> LocalNewsState -> LocalNewsState
setLastReportTimestamp time state =
  state
    { lastReportAt = Just time
    }

markNewsAsSeen :: [NewsEntry] -> LocalNewsState -> LocalNewsState
markNewsAsSeen newsEntries state =
  state
    { seenNewsIds = seenNewsIds state `Set.union` Set.fromList ((.id) <$> newsEntries)
    }

getNewsStateFilePath :: IO (Path' Abs File')
getNewsStateFilePath = getUserCacheDir <&> (</> [relfile|news.json|]) . getWaspCacheDir

ensureNewsStateFileParentDirExists :: IO ()
ensureNewsStateFileParentDirExists = do
  parentDir <- parent <$> getNewsStateFilePath
  SD.createDirectoryIfMissing True $ fromAbsDir parentDir
