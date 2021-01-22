{-# LANGUAGE DeriveGeneric #-}

module Command.Telemetry.Project
    ( getWaspProjectPathHash
    , considerSendingData
    ) where

import           Command.Common            (findWaspProjectRootDirFromCwd)
import           Control.Monad             (void, when)
import           Crypto.Hash               (SHA256 (..), hashWith)
import           Data.Aeson                ((.=))
import           Control.Monad.IO.Class    (liftIO)
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
import           Command.Telemetry.Common  (TelemetryCacheDir)
import           Command.Telemetry.User    (UserSignature (..))
import           StrongPath                (Abs, Dir, File, Path)
import qualified StrongPath                as SP


considerSendingData :: Path Abs (Dir TelemetryCacheDir) -> UserSignature -> ProjectHash -> IO ()
considerSendingData telemetryCacheDirPath userSignature projectHash = do
    projectCache <- liftIO $ readOrCreateProjectTelemetryFile telemetryCacheDirPath projectHash
    shouldSendData <- liftIO $ case _lastCheckIn projectCache of
        Nothing -> return True
        Just lastCheckIn -> do
            now <- T.getCurrentTime
            let secondsSinceLastCheckIn = T.nominalDiffTimeToSeconds (now `T.diffUTCTime` lastCheckIn)
            return $ let numSecondsInHour = 3600
                      in secondsSinceLastCheckIn > 12 * numSecondsInHour
    when shouldSendData $ do
        liftIO $ sendTelemetryData $ getProjectTelemetryData userSignature projectHash
        now <- liftIO T.getCurrentTime
        let projectCache' = projectCache { _lastCheckIn = Just now }
        liftIO $ writeProjectTelemetryFile telemetryCacheDirPath projectHash projectCache'

-- * Project hash.

newtype ProjectHash = ProjectHash { _projectHashValue :: String } deriving (Show)

getWaspProjectPathHash :: Command ProjectHash
getWaspProjectPathHash = ProjectHash . take 16 . sha256 . SP.toFilePath <$> findWaspProjectRootDirFromCwd
  where
    sha256 :: String -> String
    sha256 = show . hashWith SHA256 . ByteStringUTF8.fromString

-- * Project telemetry cache.

data ProjectTelemetryCache = ProjectTelemetryCache
    { _lastCheckIn :: Maybe T.UTCTime }
    deriving (Generic, Show)

instance Aeson.ToJSON ProjectTelemetryCache
instance Aeson.FromJSON ProjectTelemetryCache

initialCache :: ProjectTelemetryCache
initialCache = ProjectTelemetryCache { _lastCheckIn = Nothing }

-- * Project telemetry cache file.

readOrCreateProjectTelemetryFile :: Path Abs (Dir TelemetryCacheDir) -> ProjectHash -> IO ProjectTelemetryCache
readOrCreateProjectTelemetryFile telemetryCacheDirPath projectHash = do
    fileExists <- SD.doesFileExist filePathFP
    maybeCache <- if fileExists then readCacheFile else return Nothing
    case maybeCache of
        Just cache -> return cache
        Nothing -> writeProjectTelemetryFile telemetryCacheDirPath projectHash initialCache >> return initialCache
  where
      filePathFP = SP.toFilePath $ getProjectTelemetryFilePath telemetryCacheDirPath projectHash
      readCacheFile = Aeson.decode . ByteStringLazyUTF8.fromString <$> readFile filePathFP

writeProjectTelemetryFile :: Path Abs (Dir TelemetryCacheDir) -> ProjectHash -> ProjectTelemetryCache -> IO ()
writeProjectTelemetryFile telemetryCacheDirPath projectHash cache = do
    let filePathFP = SP.toFilePath $ getProjectTelemetryFilePath telemetryCacheDirPath projectHash
    writeFile filePathFP (ByteStringLazyUTF8.toString $ Aeson.encode cache)

getProjectTelemetryFilePath :: Path Abs (Dir TelemetryCacheDir) -> ProjectHash -> Path Abs File
getProjectTelemetryFilePath telemetryCacheDir (ProjectHash projectHash) =
    telemetryCacheDir SP.</> fromJust (SP.parseRelFile $ "project-" ++ projectHash)

-- * Telemetry data.

data ProjectTelemetryData = ProjectTelemetryData
    { _userSignature :: UserSignature
    , _projectHash   :: ProjectHash
    , _waspVersion   :: String
    , _os            :: String
    } deriving (Show)

getProjectTelemetryData :: UserSignature -> ProjectHash -> ProjectTelemetryData
getProjectTelemetryData userSignature projectHash = ProjectTelemetryData
    { _userSignature = userSignature
    , _projectHash = projectHash
    , _waspVersion = showVersion version
    , _os = System.Info.os
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
                ]
            ]
        request = HTTP.setRequestBodyJSON reqBodyJson $
                  HTTP.parseRequest_ "POST https://app.posthog.com/capture"
    void $ HTTP.httpNoBody request


