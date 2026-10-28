module E2e.Paths
  ( GitRootDir,
    WaspcDir,
    TestOutputsDir,
    TestDir,
    TestLogFile,
    SnapshotsDir,
    SnapshotDir,
    getWaspcDir,
    gitRootDirFromWaspcDir,
    getTestOutputsDir,
    testDirInTestOutputsDir,
    testLogFileInTestDir,
    snapshotsDirInTestOutputsDir,
    currentSnapshotDirInSnapshotsDir,
    snapshotLogFileInSnapshotsDir,
    getGoldenSnapshotsDir,
    goldenSnapshotDirInSnapshotsDir,
  )
where

import Control.Monad (unless)
import Data.Maybe (fromJust)
import StrongPath (Abs, Dir, File, Path', Rel, parseRelDir, parseRelFile, reldir, relfile, (</>))
import qualified StrongPath as SP
import System.Directory (getCurrentDirectory)
import System.FilePath (joinPath, takeFileName)

data GitRootDir

data WaspcDir

-- | All outputs of a test run: test dirs, current snapshots and logs.
data TestOutputsDir

-- | The working directory of a plain test.
data TestDir

data TestLogFile

-- | Contains the current snapshot dirs.
data SnapshotsDir

-- | A golden (expected) or a current (actual) snapshot of a snapshot test.
data SnapshotDir

getWaspcDir :: IO (Path' Abs (Dir WaspcDir))
getWaspcDir = do
  -- NOTE: Cabal runs test suites from the package root, which is the waspc dir.
  absCwd <- getCurrentDirectory
  -- A safeguard, since tests delete their output dirs.
  unless (takeFileName absCwd == "waspc") (error "Expecting the e2e tests to be run from the waspc dir")
  SP.parseAbsDir absCwd

gitRootDirFromWaspcDir :: Path' (Rel WaspcDir) (Dir GitRootDir)
gitRootDirFromWaspcDir = [reldir|..|]

getTestOutputsDir :: IO (Path' Abs (Dir TestOutputsDir))
getTestOutputsDir = (</> [reldir|e2e-tests-reimagined/test-outputs|]) <$> getWaspcDir

-- | e.g. @tests/wasp-build/fail-sqlite-project/@
testDirInTestOutputsDir :: [String] -> Path' (Rel TestOutputsDir) (Dir TestDir)
testDirInTestOutputsDir testNameParts = fromJust . parseRelDir $ joinPath ("tests" : testNameParts)

testLogFileInTestDir :: Path' (Rel TestDir) (File TestLogFile)
testLogFileInTestDir = [relfile|output.log|]

snapshotsDirInTestOutputsDir :: Path' (Rel TestOutputsDir) (Dir SnapshotsDir)
snapshotsDirInTestOutputsDir = [reldir|snapshots|]

currentSnapshotDirInSnapshotsDir :: String -> Path' (Rel SnapshotsDir) (Dir SnapshotDir)
currentSnapshotDirInSnapshotsDir snapshotName = fromJust . parseRelDir $ snapshotName ++ "-current"

-- | Kept next to the snapshot dir so it doesn't end up in the snapshot.
snapshotLogFileInSnapshotsDir :: String -> Path' (Rel SnapshotsDir) (File TestLogFile)
snapshotLogFileInSnapshotsDir snapshotName = fromJust . parseRelFile $ snapshotName ++ ".log"

-- | NOTE: Until the old e2e tests are removed, both suites share the golden
-- snapshots that live in the old suite's directory.
getGoldenSnapshotsDir :: IO (Path' Abs (Dir SnapshotsDir))
getGoldenSnapshotsDir = (</> [reldir|e2e-tests/test-outputs/snapshots|]) <$> getWaspcDir

goldenSnapshotDirInSnapshotsDir :: String -> Path' (Rel SnapshotsDir) (Dir SnapshotDir)
goldenSnapshotDirInSnapshotsDir snapshotName = fromJust . parseRelDir $ snapshotName ++ "-golden"
