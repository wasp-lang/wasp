{-# LANGUAGE DeriveGeneric #-}

module Wasp.Generator.WaspInfo
  ( persist,
    isCompatibleWithExistingBuildAt,
    WaspInfo (..),
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
import Wasp.Util.IO (doesFileExist)

data WaspInfo = WaspInfo
  { waspVersion :: String,
    generatedAt :: UTCTime,
    buildType :: BuildType
  }
  deriving (Eq, Show, Generic)

instance FromJSON WaspInfo

instance ToJSON WaspInfo

instance Inspectable WaspInfo where
  inspect WaspInfo {waspVersion, generatedAt, buildType} =
    [ InspectionEntry
        "Build"
        [ ("Wasp version", waspVersion),
          ("Generated at", show generatedAt),
          ("Build type", show buildType)
        ]
    ]

data WaspInfoFile

waspInfoInGeneratedAppDir :: Path' (Rel GeneratedAppDir) (File WaspInfoFile)
waspInfoInGeneratedAppDir = [relfile|.waspinfo|]

persist :: Path' Abs (Dir GeneratedAppDir) -> BuildType -> IO ()
persist generatedAppDir currentBuildType = do
  encodeFile (toFilePath waspInfoFile) . generateWaspInfo =<< getCurrentTime
  where
    generateWaspInfo currentTime =
      WaspInfo
        { waspVersion = currentVersion,
          generatedAt = currentTime,
          buildType = currentBuildType
        }

    waspInfoFile = generatedAppDir </> waspInfoInGeneratedAppDir
    currentVersion = showVersion Paths_waspc.version

isCompatibleWithExistingBuildAt :: BuildType -> Path' Abs (Dir GeneratedAppDir) -> IO Bool
currentBuildType `isCompatibleWithExistingBuildAt` outDir =
  either (const False) isCompatible <$> safeRead outDir
  where
    isCompatible (WaspInfo {waspVersion = storedVersion, buildType = storedBuildType}) =
      (storedVersion == currentVersion) && (storedBuildType == currentBuildType)

    currentVersion = showVersion Paths_waspc.version

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
