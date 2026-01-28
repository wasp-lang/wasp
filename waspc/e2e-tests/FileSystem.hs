module FileSystem
  ( GitRootDir,
    WaspcDir,
    SeedsDir,
    SeedsFile,
    seedsDirInWaspProjectDir,
    mainWaspFileInWaspProjectDir,
    seedsFileInSeedsDir,
    TestCaseDir,
    getTestCaseDir,
    getTestOutputsDir,
    SnapshotType (..),
    SnapshotsDir,
    SnapshotDir,
    SnapshotFileListManifestFile,
    SnapshotFile,
    getSnapshotsDir,
    snapshotDirInSnapshotsDir,
    snapshotDirInGitRootDir,
    gitRootFromSnapshotDir,
    snapshotFileListManifestFileInSnapshotDir,
  )
where

import Control.Monad (unless)
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import StrongPath (Abs, Dir, File, File', Path', Rel, parseRelDir, parseRelFile, reldir, relfile, (</>))
import qualified StrongPath as SP
import System.Directory (getCurrentDirectory)
import System.FilePath (takeFileName)
import Wasp.Project (WaspProjectDir)

data GitRootDir

data WaspcDir

-- | The directory where all test outputs are created.
data TestOutputsDir

getTestOutputsDir :: IO (Path' Abs (Dir TestOutputsDir))
getTestOutputsDir = getWaspcDirPath <&> (</> testsOutputsDirInWaspcDir)

getWaspcDirPath :: IO (Path' Abs (Dir WaspcDir))
getWaspcDirPath = do
  -- NOTE: Cabal launches `cabal test` from root of the project, so this should always be some absolute path to waspc.
  absCwd <- getCurrentDirectory
  -- Just a little extra safeguard here since we are doing destructive file ops.
  unless (takeFileName absCwd == "waspc") (error "Expecting test process to be invoked from waspc dir")
  SP.parseAbsDir absCwd

testsOutputsDirInWaspcDir :: Path' (Rel WaspcDir) (Dir TestOutputsDir)
testsOutputsDirInWaspcDir = [reldir|e2e-tests/test-outputs|]

waspcDirInGitRootDir :: Path' (Rel GitRootDir) (Dir WaspcDir)
waspcDirInGitRootDir = [reldir|waspc|]

-- WaspProject file system

data SeedsDir

data SeedsFile

seedsDirInWaspProjectDir :: Path' (Rel WaspProjectDir) (Dir SeedsDir)
seedsDirInWaspProjectDir = [reldir|src/db|]

seedsFileInSeedsDir :: String -> Path' (Rel SeedsDir) File'
seedsFileInSeedsDir = fromJust . parseRelFile

mainWaspFileInWaspProjectDir :: Path' (Rel WaspProjectDir) File'
mainWaspFileInWaspProjectDir = [relfile|main.wasp|]

-- 'Test' tests file system

-- | A directory inside the 'TestOutputsDir' where outputs of a specific e2e test case are stored.
data TestCaseDir

-- | Get the directory for a specific test case: test-outputs/{testName}/{testCaseName}
getTestCaseDir :: String -> String -> IO (Path' Abs (Dir TestCaseDir))
getTestCaseDir testName testCaseName = do
  testOutputsDir <- getTestOutputsDir
  return $ testOutputsDir </> testCaseDirInTestOutputsDir (testName ++ "/" ++ testCaseName)

testCaseDirInTestOutputsDir :: String -> Path' (Rel TestOutputsDir) (Dir TestCaseDir)
testCaseDirInTestOutputsDir = fromJust . parseRelDir

-- 'SnapshotTest' tests file system
--
data SnapshotType = Golden | Current

instance Show SnapshotType where
  show Golden = "golden"
  show Current = "current"

-- | The directory inside 'TestOutputsDir' where all `SnapshotDir`s are stored.
data SnapshotsDir

-- | The directory inside a 'SnapshotsDir' where a snapshot is stored.
-- The snapshot is either a "golden" (expected) or a "current" (actual) snapshot.
-- It follows a @<snapshot-test-name>-<snapshot-type>@ naming convention.
-- e.g. @testName-golden@, @kitchen-sink-current@
data SnapshotDir

-- | Represent any file inside of a 'SnapshotDir'.
data SnapshotFile

-- | The file inside of a 'SnapshotDir'.
-- Lists all files that should exist in the 'SnapshotDir' directory.
data SnapshotFileListManifestFile

getSnapshotsDir :: IO (Path' Abs (Dir SnapshotsDir))
getSnapshotsDir = getTestOutputsDir <&> (</> snapshotsDirInE2eTests)

snapshotsDirInE2eTests :: Path' (Rel TestOutputsDir) (Dir SnapshotsDir)
snapshotsDirInE2eTests = [reldir|snapshots|]

snapshotDirInSnapshotsDir :: String -> SnapshotType -> Path' (Rel SnapshotsDir) (Dir SnapshotDir)
snapshotDirInSnapshotsDir snapshotTestName snapshotType = (fromJust . parseRelDir) (snapshotTestName ++ "-" ++ show snapshotType)

snapshotFileListManifestFileInSnapshotDir :: Path' (Rel SnapshotDir) (File SnapshotFileListManifestFile)
snapshotFileListManifestFileInSnapshotDir = [relfile|snapshot-file-list.manifest|]

-- | Inverse of 'gitRootFromSnapshotDir'.
-- NOTE: If you change this function, change the other one too.
snapshotDirInGitRootDir :: String -> SnapshotType -> Path' (Rel GitRootDir) (Dir SnapshotDir)
snapshotDirInGitRootDir snapshotTestName snapshotType =
  waspcDirInGitRootDir
    </> testsOutputsDirInWaspcDir
    </> snapshotsDirInE2eTests
    </> snapshotDirInSnapshotsDir snapshotTestName snapshotType

-- | Inverse of 'snapshotDirInGitRootDir'.
-- NOTE: If you change this function, change the other one too.
gitRootFromSnapshotDir :: Path' (Rel SnapshotDir) (Dir GitRootDir)
gitRootFromSnapshotDir = [reldir|../../../../../|]
