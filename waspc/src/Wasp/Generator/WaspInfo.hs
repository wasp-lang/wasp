{-# LANGUAGE DeriveGeneric #-}

module Wasp.Generator.WaspInfo
  ( persist,
    isCompatibleWithExistingBuildAt,
    WaspInfo (..),
    safeRead,
  )
where

import Data.Aeson (ToJSON, decodeFileStrict, encodeFile)
import Data.Aeson.Types (FromJSON)
import Data.Time (UTCTime, getCurrentTime)
import Data.Version (showVersion)
import GHC.Generics (Generic)
import qualified Paths_waspc
import StrongPath (Abs, Dir, File, Path', Rel, relfile, toFilePath, (</>))
import Wasp.Generator.Common (ProjectRootDir)
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

data WaspInfoFile

waspInfoInProjectRootDir :: Path' (Rel ProjectRootDir) (File WaspInfoFile)
waspInfoInProjectRootDir = [relfile|.waspinfo|]

persist :: Path' Abs (Dir ProjectRootDir) -> BuildType -> IO ()
persist projectRootDir currentBuildType = do
  encodeFile (toFilePath waspInfoFile) . generateWaspInfo =<< getCurrentTime
  where
    generateWaspInfo currentTime =
      WaspInfo
        { waspVersion = currentVersion,
          generatedAt = currentTime,
          buildType = currentBuildType
        }

    waspInfoFile = projectRootDir </> waspInfoInProjectRootDir
    currentVersion = showVersion Paths_waspc.version

isCompatibleWithExistingBuildAt :: BuildType -> Path' Abs (Dir ProjectRootDir) -> IO Bool
currentBuildType `isCompatibleWithExistingBuildAt` outDir =
  maybe False isCompatible <$> safeRead outDir
  where
    isCompatible (WaspInfo {waspVersion = storedVersion, buildType = storedBuildType}) =
      (storedVersion == currentVersion) && (storedBuildType == currentBuildType)

    currentVersion = showVersion Paths_waspc.version

safeRead :: Path' Abs (Dir ProjectRootDir) -> IO (Maybe WaspInfo)
safeRead projectRootDir = do
  let waspInfoFile = projectRootDir </> waspInfoInProjectRootDir

  doesFileExist waspInfoFile >>= \case
    False -> return Nothing
    True -> decodeFileStrict $ toFilePath waspInfoFile
