{-# LANGUAGE DeriveGeneric #-}

module Command.Telemetry.Project
    ( getWaspProjectPathHash
    , considerSendingData
    , readProjectTelemetryFile
    , getTimeOfLastTelemetryDataSent
    ) where

import           Command.Common            (findWaspProjectRootDirFromCwd)
import           Control.Monad             (void, when)
import           Crypto.Hash               (SHA256 (..), hashWith)
import           Data.Aeson                ((.=))
import qualified Data.Aeson                as Aeson
import qualified Data.ByteString.Lazy.UTF8 as ByteStringLazyUTF8
import qualified Data.ByteString.UTF8      as ByteStringUTF8
import           Data.Maybe                (fromJust)
import qualified Data.Time                 as T
import           Data.Version              (showVersion)
import           GHC.Generics
import qualified Network.HTTP.Simple       as HTTP
import           Paths_waspc               (version)
import qualified System.Directory          as SD
import qualified System.Info

import           Command                   (Command)
import qualified Command.Call
import           Command.Telemetry.Common  (TelemetryCacheDir)
import           Command.Telemetry.User    (UserSignature (..))
import           StrongPath                (Abs, Dir, File, Path)
import qualified StrongPath                as SP


considerSendingData :: Path Abs (Dir TelemetryCacheDir) -> UserSignature -> ProjectHash -> Command.Call.Call -> IO ()
considerSendingData telemetryCacheDirPath userSignature projectHash cmdCall = do
    projectCache <- readOrCreateProjectTelemetryFile telemetryCacheDirPath projectHash

    let relevantLastCheckIn = case cmdCall of
            Command.Call.Build -> _lastCheckInBuild projectCache
            _                  -> _lastCheckIn projectCache

    shouldSendData <- case relevantLastCheckIn of
        Nothing          -> return True
        Just lastCheckIn -> isOlderThan12Hours lastCheckIn

    when shouldSendData $ do
        sendTelemetryData $ getProjectTelemetryData userSignature projectHash cmdCall
        projectCache' <- newProjectCache projectCache
        writeProjectTelemetryFile telemetryCacheDirPath projectHash projectCache'
  where
      isOlderThan12Hours :: T.UTCTime -> IO Bool
      isOlderThan12Hours time = do
          now <- T.getCurrentTime
          let secondsSinceLastCheckIn = T.nominalDiffTimeToSeconds (now `T.diffUTCTime` time)
          return $ let numSecondsInHour = 3600
                   in secondsSinceLastCheckIn > 12 * numSecondsInHour

      newProjectCache :: ProjectTelemetryCache -> IO ProjectTelemetryCache
      newProjectCache currentProjectCache = do
          now <- T.getCurrentTime
          return currentProjectCache
              { _lastCheckIn      = Just now
              , _lastCheckInBuild = case cmdCall of
                      Command.Call.Build -> Just now
                      _                  -> _lastCheckInBuild currentProjectCache
              }

-- * Project hash.

newtype ProjectHash = ProjectHash { _projectHashValue :: String } deriving (Show)

getWaspProjectPathHash :: Command ProjectHash
getWaspProjectPathHash = ProjectHash . take 16 . sha256 . SP.toFilePath <$> findWaspProjectRootDirFromCwd
  where
    sha256 :: String -> String
    sha256 = show . hashWith SHA256 . ByteStringUTF8.fromString

-- * Project telemetry cache.

data ProjectTelemetryCache = ProjectTelemetryCache
    { _lastCheckIn      :: Maybe T.UTCTime -- Last time when CLI was called for this project, any command.
    , _lastCheckInBuild :: Maybe T.UTCTime -- Last time when CLI was called for this project, with Build command.
    }
    deriving (Generic, Show)

instance Aeson.ToJSON ProjectTelemetryCache
instance Aeson.FromJSON ProjectTelemetryCache

initialCache :: ProjectTelemetryCache
initialCache = ProjectTelemetryCache { _lastCheckIn = Nothing, _lastCheckInBuild = Nothing }

-- * Project telemetry cache file.

getTimeOfLastTelemetryDataSent :: ProjectTelemetryCache -> Maybe T.UTCTime
getTimeOfLastTelemetryDataSent cache = maximum [_lastCheckIn cache, _lastCheckInBuild cache]

readProjectTelemetryFile :: Path Abs (Dir TelemetryCacheDir) -> ProjectHash -> IO (Maybe ProjectTelemetryCache)
readProjectTelemetryFile telemetryCacheDirPath projectHash = do
    fileExists <- SD.doesFileExist filePathFP
    if fileExists then readCacheFile else return Nothing
  where
      filePathFP = SP.toFilePath $ getProjectTelemetryFilePath telemetryCacheDirPath projectHash
      readCacheFile = Aeson.decode . ByteStringLazyUTF8.fromString <$> readFile filePathFP

readOrCreateProjectTelemetryFile :: Path Abs (Dir TelemetryCacheDir) -> ProjectHash -> IO ProjectTelemetryCache
readOrCreateProjectTelemetryFile telemetryCacheDirPath projectHash = do
    maybeProjectTelemetryCache <- readProjectTelemetryFile telemetryCacheDirPath projectHash
    case maybeProjectTelemetryCache of
        Just cache -> return cache
        Nothing    -> writeProjectTelemetryFile telemetryCacheDirPath projectHash initialCache >> return initialCache

writeProjectTelemetryFile :: Path Abs (Dir TelemetryCacheDir) -> ProjectHash -> ProjectTelemetryCache -> IO ()
writeProjectTelemetryFile telemetryCacheDirPath projectHash cache = do
    writeFile filePathFP (ByteStringLazyUTF8.toString $ Aeson.encode cache)
  where
    filePathFP = SP.toFilePath $ getProjectTelemetryFilePath telemetryCacheDirPath projectHash

getProjectTelemetryFilePath :: Path Abs (Dir TelemetryCacheDir) -> ProjectHash -> Path Abs File
getProjectTelemetryFilePath telemetryCacheDir (ProjectHash projectHash) =
    telemetryCacheDir SP.</> fromJust (SP.parseRelFile $ "project-" ++ projectHash)

-- * Telemetry data.

data ProjectTelemetryData = ProjectTelemetryData
    { _userSignature :: UserSignature
    , _projectHash   :: ProjectHash
    , _waspVersion   :: String
    , _os            :: String
    , _isBuild       :: Bool
    } deriving (Show)

getProjectTelemetryData :: UserSignature -> ProjectHash -> Command.Call.Call -> ProjectTelemetryData
getProjectTelemetryData userSignature projectHash cmdCall = ProjectTelemetryData
    { _userSignature = userSignature
    , _projectHash = projectHash
    , _waspVersion = showVersion version
    , _os = System.Info.os
    , _isBuild = case cmdCall of
            Command.Call.Build -> True
            _                  -> False
    }

sendTelemetryData :: ProjectTelemetryData -> IO ()
sendTelemetryData telemetryData = do
    let reqBodyJson = Aeson.object
            [ -- PostHog api_key is public so it is ok that we have it here.
              "api_key" .= ("CdDd2A0jKTI2vFAsrI9JWm3MqpOcgHz1bMyogAcwsE4" :: String)
            , "event" .= ("cli" :: String)
            , "properties" .= Aeson.object
                [ -- distinct_id is special PostHog value, used as user id.
                  "distinct_id" .= _userSignatureValue (_userSignature telemetryData)
                -- Following are our custom metrics:
                , "project_hash" .= _projectHashValue (_projectHash telemetryData)
                , "wasp_version" .= _waspVersion telemetryData
                , "os" .= _os telemetryData
                , "is_build" .= _isBuild telemetryData
                ]
            ]
        request = HTTP.setRequestBodyJSON reqBodyJson $
                  HTTP.parseRequest_ "POST https://app.posthog.com/capture"
    void $ HTTP.httpNoBody request
