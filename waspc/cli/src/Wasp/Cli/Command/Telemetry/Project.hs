{-# LANGUAGE DeriveGeneric #-}

module Wasp.Cli.Command.Telemetry.Project
  ( getWaspProjectPathHash,
    considerSendingData,
    readProjectTelemetryCacheFile,
    getTimeOfLastTelemetryDataSent,
    -- NOTE: for testing only
    checkIfEnvValueIsTruthy,
  )
where

import Control.Monad (void, when)
import Control.Monad.Except (catchError)
import Crypto.Hash (SHA256 (..), hashWith)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.UTF8 as ByteStringLazyUTF8
import qualified Data.ByteString.UTF8 as ByteStringUTF8
import Data.Char (toLower)
import Data.Functor ((<&>))
import Data.List (intercalate, intersect)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Time as T
import Data.Version (showVersion)
import GHC.Generics
import qualified Network.HTTP.Simple as HTTP
import Paths_waspc (version)
import StrongPath (Abs, Dir, File', Path')
import qualified StrongPath as SP
import qualified System.Environment as ENV
import qualified System.IO as SIO
import qualified System.Info
import Wasp.Cli.Command (Command)
import qualified Wasp.Cli.Command.Call as Command.Call
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import Wasp.Cli.Command.Telemetry.Common (TelemetryCacheDir)
import Wasp.Cli.Command.Telemetry.User (UserSignature (..))
import Wasp.Util (ifM)
import qualified Wasp.Util.IO as IOUtil

considerSendingData :: Path' Abs (Dir TelemetryCacheDir) -> UserSignature -> ProjectHash -> Command.Call.Call -> IO ()
considerSendingData telemetryCacheDirPath userSignature projectHash cmdCall = do
  projectCache <- readOrCreateProjectTelemetryFile telemetryCacheDirPath projectHash

  let relevantLastCheckIn = case cmdCall of
        Command.Call.Build -> _lastCheckInBuild projectCache
        Command.Call.Deploy _ -> _lastCheckInDeploy projectCache
        _ -> _lastCheckIn projectCache

  shouldSendData <- case relevantLastCheckIn of
    Nothing -> return True
    Just lastCheckIn -> isOlderThan12Hours lastCheckIn

  when shouldSendData $ do
    telemetryContext <- getTelemetryContext
    sendTelemetryData $ getProjectTelemetryData userSignature projectHash cmdCall telemetryContext
    projectCache' <- newProjectCache projectCache
    writeProjectTelemetryFile telemetryCacheDirPath projectHash projectCache'
  where
    isOlderThan12Hours :: T.UTCTime -> IO Bool
    isOlderThan12Hours time = do
      now <- T.getCurrentTime
      let secondsSinceLastCheckIn = T.nominalDiffTimeToSeconds (now `T.diffUTCTime` time)
      return $
        let numSecondsInHour = 3600
         in secondsSinceLastCheckIn > 12 * numSecondsInHour

    newProjectCache :: ProjectTelemetryCache -> IO ProjectTelemetryCache
    newProjectCache currentProjectCache = do
      now <- T.getCurrentTime
      return
        currentProjectCache
          { _lastCheckIn = Just now,
            _lastCheckInBuild = case cmdCall of
              Command.Call.Build -> Just now
              _ -> _lastCheckInBuild currentProjectCache
          }

getTelemetryContext :: IO String
getTelemetryContext = do
  unwords . filter (not . null)
    <$> sequence
      [ fromMaybe "" <$> ENV.lookupEnv "WASP_TELEMETRY_CONTEXT",
        checkIfOnCi <&> \case True -> "CI"; False -> "",
        -- NOTE(martin): We are not 100% sure how correctly does `hIsTerminalDevice` detect
        --   if shell is non-interactive or not, so let's use this info with regard to that and
        --   revise this in the future once we have more data.
        (SIO.hIsTerminalDevice SIO.stdout `catchError` const (return True))
          <&> \case True -> ""; False -> "NON_INTERACTIVE_SHELL"
      ]
  where
    -- This function was inspired by https://github.com/watson/ci-info/blob/master/index.js .
    checkIfOnCi :: IO Bool
    checkIfOnCi =
      any checkIfEnvValueIsTruthy
        <$> mapM
          ENV.lookupEnv
          [ "BUILD_ID", -- Jenkins, Codeship
            "BUILD_NUMBER", -- Jenkins, TeamCity
            "CI", -- Github actions, Travis CI, CircleCI, Cirrus CI, Gitlab CI, Appveyor, Codeship, dsari
            "CI_APP_ID", -- Appflow
            "CI_BUILD_ID", -- Appflow
            "CI_BUILD_NUMBER", -- Appflow
            "CI_NAME", -- Codeship and others
            "CONTINUOUS_INTEGRATION", -- Travis CI, Cirrus CI
            "RUN_ID" -- TaskCluster, dsari
          ]

checkIfEnvValueIsTruthy :: Maybe String -> Bool
checkIfEnvValueIsTruthy Nothing = False
checkIfEnvValueIsTruthy (Just v)
  | null v = False
  | (toLower <$> v) == "false" = False
  | otherwise = True

-- * Project hash.

newtype ProjectHash = ProjectHash {_projectHashValue :: String} deriving (Show)

getWaspProjectPathHash :: Command ProjectHash
getWaspProjectPathHash = do
  InWaspProject waspRoot <- require
  return . ProjectHash . take 16 . sha256 . SP.toFilePath $ waspRoot
  where
    sha256 :: String -> String
    sha256 = show . hashWith SHA256 . ByteStringUTF8.fromString

-- * Project telemetry cache.

data ProjectTelemetryCache = ProjectTelemetryCache
  { _lastCheckIn :: Maybe T.UTCTime, -- Last time when CLI was called for this project, any command.
    _lastCheckInBuild :: Maybe T.UTCTime, -- Last time when CLI was called for this project, with Build command.
    _lastCheckInDeploy :: Maybe T.UTCTime -- Last time when CLI was called for this project, with Deploy command.
  }
  deriving (Generic, Show)

instance Aeson.ToJSON ProjectTelemetryCache

instance Aeson.FromJSON ProjectTelemetryCache

initialCache :: ProjectTelemetryCache
initialCache = ProjectTelemetryCache {_lastCheckIn = Nothing, _lastCheckInBuild = Nothing, _lastCheckInDeploy = Nothing}

-- * Project telemetry cache file.

getTimeOfLastTelemetryDataSent :: ProjectTelemetryCache -> Maybe T.UTCTime
getTimeOfLastTelemetryDataSent cache = maximum [_lastCheckIn cache, _lastCheckInBuild cache]

readProjectTelemetryCacheFile :: Path' Abs (Dir TelemetryCacheDir) -> ProjectHash -> IO (Maybe ProjectTelemetryCache)
readProjectTelemetryCacheFile telemetryCacheDirPath projectHash =
  ifM
    (IOUtil.doesFileExist projectTelemetryFile)
    parseProjectTelemetryFile
    (return Nothing)
  where
    projectTelemetryFile = getProjectTelemetryFilePath telemetryCacheDirPath projectHash
    parseProjectTelemetryFile = Aeson.decode . ByteStringLazyUTF8.fromString <$> IOUtil.readFile projectTelemetryFile

readOrCreateProjectTelemetryFile :: Path' Abs (Dir TelemetryCacheDir) -> ProjectHash -> IO ProjectTelemetryCache
readOrCreateProjectTelemetryFile telemetryCacheDirPath projectHash = do
  maybeProjectTelemetryCache <- readProjectTelemetryCacheFile telemetryCacheDirPath projectHash
  case maybeProjectTelemetryCache of
    Just cache -> return cache
    Nothing -> writeProjectTelemetryFile telemetryCacheDirPath projectHash initialCache >> return initialCache

writeProjectTelemetryFile :: Path' Abs (Dir TelemetryCacheDir) -> ProjectHash -> ProjectTelemetryCache -> IO ()
writeProjectTelemetryFile telemetryCacheDirPath projectHash cache = do
  IOUtil.writeFile projectTelemetryFile (ByteStringLazyUTF8.toString $ Aeson.encode cache)
  where
    projectTelemetryFile = getProjectTelemetryFilePath telemetryCacheDirPath projectHash

getProjectTelemetryFilePath :: Path' Abs (Dir TelemetryCacheDir) -> ProjectHash -> Path' Abs File'
getProjectTelemetryFilePath telemetryCacheDir (ProjectHash projectHash) =
  telemetryCacheDir SP.</> fromJust (SP.parseRelFile $ "project-" ++ projectHash)

-- * Telemetry data.

data ProjectTelemetryData = ProjectTelemetryData
  { _userSignature :: UserSignature,
    _projectHash :: ProjectHash,
    _waspVersion :: String,
    _os :: String,
    _isBuild :: Bool,
    _deployCmdArgs :: String,
    _context :: String
  }
  deriving (Show)

getProjectTelemetryData :: UserSignature -> ProjectHash -> Command.Call.Call -> String -> ProjectTelemetryData
getProjectTelemetryData userSignature projectHash cmdCall context =
  ProjectTelemetryData
    { _userSignature = userSignature,
      _projectHash = projectHash,
      _waspVersion = showVersion version,
      _os = System.Info.os,
      _isBuild = case cmdCall of
        Command.Call.Build -> True
        _ -> False,
      _deployCmdArgs = case cmdCall of
        Command.Call.Deploy deployCmdArgs -> intercalate ";" $ extractKeyDeployArgs deployCmdArgs
        _ -> "",
      _context = context
    }

-- | To preserve user's privacy, we capture only args (from `wasp deploy ...` cmd)
-- that are from the predefined set of keywords.
-- NOTE: If you update the list of keywords here, make sure to also update them in the official docs
--   on our webpage, under Telemetry.
extractKeyDeployArgs :: [String] -> [String]
extractKeyDeployArgs = intersect ["fly", "setup", "create-db", "deploy", "cmd"]

sendTelemetryData :: ProjectTelemetryData -> IO ()
sendTelemetryData telemetryData = do
  let reqBodyJson =
        Aeson.object
          [ -- PostHog api_key is public so it is ok that we have it here.
            "api_key" .= ("CdDd2A0jKTI2vFAsrI9JWm3MqpOcgHz1bMyogAcwsE4" :: String),
            "event" .= ("cli" :: String),
            "properties"
              .= Aeson.object
                [ -- distinct_id is special PostHog value, used as user id.
                  "distinct_id" .= _userSignatureValue (_userSignature telemetryData),
                  -- Following are our custom metrics:
                  "project_hash" .= _projectHashValue (_projectHash telemetryData),
                  "wasp_version" .= _waspVersion telemetryData,
                  "os" .= _os telemetryData,
                  "is_build" .= _isBuild telemetryData,
                  "deploy_cmd_args" .= _deployCmdArgs telemetryData,
                  "context" .= _context telemetryData
                ]
          ]
      request =
        HTTP.setRequestBodyJSON reqBodyJson $
          HTTP.parseRequest_ "POST https://app.posthog.com/capture"
  void $ HTTP.httpNoBody request
