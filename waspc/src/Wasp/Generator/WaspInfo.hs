{-# LANGUAGE DeriveGeneric #-}

module Wasp.Generator.WaspInfo
  ( persist,
    remove,
    isCompatibleWithExistingBuildAt,
    isCompleteBuildAt,
    WaspInfo (..),
    SetupStatus (..),
    safeRead,
    ReadResult,
    ReadError (..),
  )
where

import Data.Aeson (ToJSON, decodeFileStrict, encodeFile)
import Data.Aeson.Types (FromJSON)
import Data.Time (UTCTime, getCurrentTime)
import Data.Version (showVersion)
import GHC.Generics (Generic)
import qualified Paths_waspc
import StrongPath (Abs, Dir, File, Path', Rel, relfile, toFilePath, (</>))
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Inspectable (Inspectable (inspect), InspectionEntry (InspectionEntry))
import Wasp.Project.BuildType (BuildType)
import Wasp.Util.IO (deleteFileIfExists, doesFileExist)

data WaspInfo = WaspInfo
  { waspVersion :: String,
    generatedAt :: UTCTime,
    buildType :: BuildType,
    setupStatus :: SetupStatus
  }
  deriving (Eq, Show, Generic)

instance FromJSON WaspInfo

instance ToJSON WaspInfo

instance Inspectable WaspInfo where
  inspect WaspInfo {waspVersion, generatedAt, buildType, setupStatus} =
    [ InspectionEntry
        "Build"
        [ ("Wasp version", waspVersion),
          ("Generated at", show generatedAt),
          ("Build type", show buildType),
          ("Setup", showSetupStatus setupStatus)
        ]
    ]
    where
      showSetupStatus SetupPending = "Pending"
      showSetupStatus SetupComplete = "Complete"

-- | Whether the setup step (npm install, Prisma client generation, etc.) has
-- finished successfully for the generated app.
data SetupStatus = SetupPending | SetupComplete
  deriving (Eq, Show, Generic)

instance FromJSON SetupStatus

instance ToJSON SetupStatus

data WaspInfoFile

waspInfoInGeneratedAppDir :: Path' (Rel GeneratedAppDir) (File WaspInfoFile)
waspInfoInGeneratedAppDir = [relfile|.waspinfo|]

currentVersion :: String
currentVersion = showVersion Paths_waspc.version

persist :: Path' Abs (Dir GeneratedAppDir) -> BuildType -> SetupStatus -> IO ()
persist generatedAppDir currentBuildType currentSetupStatus = do
  encodeFile (toFilePath waspInfoFile) . generateWaspInfo =<< getCurrentTime
  where
    generateWaspInfo currentTime =
      WaspInfo
        { waspVersion = currentVersion,
          generatedAt = currentTime,
          buildType = currentBuildType,
          setupStatus = currentSetupStatus
        }

    waspInfoFile = generatedAppDir </> waspInfoInGeneratedAppDir

remove :: Path' Abs (Dir GeneratedAppDir) -> IO ()
remove generatedAppDir = deleteFileIfExists $ generatedAppDir </> waspInfoInGeneratedAppDir

isCompleteBuildAt :: BuildType -> Path' Abs (Dir GeneratedAppDir) -> IO Bool
currentBuildType `isCompleteBuildAt` outDir =
  either (const False) isComplete <$> safeRead outDir
  where
    isComplete waspInfo =
      setupStatus waspInfo == SetupComplete
        && isCompatibleWith currentBuildType waspInfo

isCompatibleWithExistingBuildAt :: BuildType -> Path' Abs (Dir GeneratedAppDir) -> IO Bool
currentBuildType `isCompatibleWithExistingBuildAt` outDir =
  either (const False) (isCompatibleWith currentBuildType) <$> safeRead outDir

isCompatibleWith :: BuildType -> WaspInfo -> Bool
isCompatibleWith currentBuildType waspInfo =
  waspVersion waspInfo == currentVersion
    && buildType waspInfo == currentBuildType

type ReadResult = Either ReadError WaspInfo

data ReadError = NotFound | IncompatibleFormat

safeRead :: Path' Abs (Dir GeneratedAppDir) -> IO ReadResult
safeRead generatedAppDir =
  doesFileExist waspInfoFile >>= \case
    False -> return $ Left NotFound
    True ->
      maybe (Left IncompatibleFormat) Right
        <$> decodeFileStrict (toFilePath waspInfoFile)
  where
    waspInfoFile = generatedAppDir </> waspInfoInGeneratedAppDir
