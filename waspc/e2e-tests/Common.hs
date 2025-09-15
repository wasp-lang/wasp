module Common
  ( SnapshotType (..),
    GitRepositoryRoot,
    E2eTestsDir,
    TestOutputsDir,
    getTestOutputsDir,
    snapshotDirInTestOutputsDir,
    gitRootFromSnapshotProjectDir,
    snapshotProjectDirInSnapshotDir,
    expectedFilesManifestFileInSnapshotDir,
  )
where

import Control.Monad (unless)
import Data.Maybe (fromJust)
import StrongPath (Abs, Dir, File, Path', Rel, parseRelDir, reldir, relfile, (</>))
import qualified StrongPath as SP
import System.Directory (getCurrentDirectory)
import System.FilePath (takeFileName)

data GitRepositoryRoot

data WaspcDir

data E2eTestsDir

data TestOutputsDir

data SnapshotType = Golden | Current

instance Show SnapshotType where
  show Golden = "golden"
  show Current = "current"

-- The directory inside a TestOutputsDir where golden test snapshots are stored.
-- It follows a "<project-name>-<golden-test-type>" naming convention.
-- e.g. "wasp-new-golden", "kitchen-sink-current"
data SnapshotDir

-- The directory inside a SnapshotDir where the project for that test is located.
-- It is named after the project, e.g., "kitchen-sink".
data SnapshotProjectDir

data ExpectedFilesManifestFile

waspcDirInGitRoot :: Path' (Rel GitRepositoryRoot) (Dir WaspcDir)
waspcDirInGitRoot = [reldir|waspc|]

getWaspcDirPath :: IO (Path' Abs (Dir WaspcDir))
getWaspcDirPath = do
  -- NOTE: Cabal launches `cabal test` from root of the project, so this should always be some absolute path to waspc.
  absCwd <- getCurrentDirectory
  -- Just a little extra safeguard here since we are doing destructive file ops.
  unless (takeFileName absCwd == "waspc") (error "Expecting test process to be invoked from waspc dir")
  SP.parseAbsDir absCwd

e2eTestsDirInWaspcDir :: Path' (Rel WaspcDir) (Dir E2eTestsDir)
e2eTestsDirInWaspcDir = [reldir|e2e-tests|]

getE2eTestsDir :: IO (Path' Abs (Dir E2eTestsDir))
getE2eTestsDir = (</> e2eTestsDirInWaspcDir) <$> getWaspcDirPath

testOutputsDirInE2eTests :: Path' (Rel E2eTestsDir) (Dir TestOutputsDir)
testOutputsDirInE2eTests = [reldir|test-outputs|]

getTestOutputsDir :: IO (Path' Abs (Dir TestOutputsDir))
getTestOutputsDir = (</> testOutputsDirInE2eTests) <$> getE2eTestsDir

snapshotDirInTestOutputsDir :: String -> SnapshotType -> Path' (Rel TestOutputsDir) (Dir SnapshotDir)
snapshotDirInTestOutputsDir projectName snapshotType = (fromJust . parseRelDir) (projectName ++ "-" ++ show snapshotType)

expectedFilesManifestFileInSnapshotDir :: Path' (Rel SnapshotDir) (File ExpectedFilesManifestFile)
expectedFilesManifestFileInSnapshotDir = [relfile|expected-files.manifest|]

snapshotProjectDirInSnapshotDir :: String -> Path' (Rel SnapshotDir) (Dir SnapshotProjectDir)
snapshotProjectDirInSnapshotDir = fromJust . parseRelDir

-- | Inverse of 'gitRootFromSnapshotProjectDir'.
--   If you change this function, change the other one too.
snapshotProjectDirInGitRoot :: String -> SnapshotType -> Path' (Rel GitRepositoryRoot) (Dir SnapshotProjectDir)
snapshotProjectDirInGitRoot projectName snapshotType =
  waspcDirInGitRoot
    </> e2eTestsDirInWaspcDir
    </> testOutputsDirInE2eTests
    </> snapshotDirInTestOutputsDir projectName snapshotType
    </> snapshotProjectDirInSnapshotDir projectName

-- | Inverse of 'snapshotProjectDirInGitRoot'.
--   If you change this function, change the other one too.
gitRootFromSnapshotProjectDir :: Path' (Rel SnapshotProjectDir) (Dir GitRepositoryRoot)
gitRootFromSnapshotProjectDir = [reldir|../../../../|]
