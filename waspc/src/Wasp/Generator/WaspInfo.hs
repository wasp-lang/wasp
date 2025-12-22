{-# LANGUAGE DeriveGeneric #-}

module Wasp.Generator.WaspInfo where

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

data WaspInfoPath

waspInfoInProjectRootDir :: Path' (Rel ProjectRootDir) (File WaspInfoPath)
waspInfoInProjectRootDir = [relfile|.waspinfo|]

safeRead :: Path' Abs (Dir ProjectRootDir) -> IO (Maybe WaspInfo)
safeRead projectRootDir = do
  let waspInfoPath = projectRootDir </> waspInfoInProjectRootDir

  doesFileExist waspInfoPath >>= \case
    False -> return Nothing
    True -> decodeFileStrict $ toFilePath waspInfoPath

-- | Writes .waspinfo, which contains some basic metadata about how/when wasp generated the code.
generate :: BuildType -> IO WaspInfo
generate buildType' = do
  currentTime <- getCurrentTime
  return
    WaspInfo
      { waspVersion = showVersion Paths_waspc.version,
        generatedAt = currentTime,
        buildType = buildType'
      }

-- | Writes .waspinfo, which contains some basic metadata about how/when wasp generated the code.
write :: WaspInfo -> Path' Abs (Dir ProjectRootDir) -> IO ()
write waspInfo dstDir = do
  encodeFile
    (toFilePath $ dstDir </> waspInfoInProjectRootDir)
    waspInfo

needsCleanBuild :: BuildType -> WaspInfo -> Bool
needsCleanBuild buildType' oldInfo =
  (waspVersion oldInfo /= showVersion Paths_waspc.version)
    || (buildType oldInfo /= buildType')
