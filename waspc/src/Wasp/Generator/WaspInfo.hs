{-# LANGUAGE DeriveGeneric #-}

module Wasp.Generator.WaspInfo (persist, checkIfCleanBuildNeeded) where

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

checkIfCleanBuildNeeded :: Path' Abs (Dir ProjectRootDir) -> BuildType -> IO Bool
checkIfCleanBuildNeeded outDir currentBuildType =
  maybe True (needsCleanBuild currentBuildType) <$> safeRead outDir

safeRead :: Path' Abs (Dir ProjectRootDir) -> IO (Maybe WaspInfo)
safeRead projectRootDir = do
  let waspInfoFile = projectRootDir </> waspInfoInProjectRootDir

  doesFileExist waspInfoFile >>= \case
    False -> return Nothing
    True -> decodeFileStrict $ toFilePath waspInfoFile

needsCleanBuild :: BuildType -> WaspInfo -> Bool
needsCleanBuild currentBuildType storedInfo =
  (storedVersion /= currentVersion) || (storedBuildType /= currentBuildType)
  where
    storedVersion = waspVersion storedInfo
    currentVersion = showVersion Paths_waspc.version
    storedBuildType = buildType storedInfo
