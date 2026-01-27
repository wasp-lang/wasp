module SnapshotTest.FileSystem
  ( SnapshotType (..),
    SnapshotsDir,
    SnapshotDir,
    SnapshotFileListManifestFile,
    SnapshotFile,
    snapshotsDirInE2eTests,
    getSnapshotsDir,
    snapshotDirInSnapshotsDir,
    snapshotDirInGitRootDir,
    gitRootFromSnapshotDir,
    snapshotFileListManifestFileInSnapshotDir,
  )
where

import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import FileSystem (E2eTestsDir, GitRootDir, e2eTestsDirInWaspcDir, getE2eTestsDir, waspcDirInGitRootDir)
import SnapshotTest.Snapshot (SnapshotType (..))
import StrongPath

-- | The directory where all snapshots are stored.
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

snapshotsDirInE2eTests :: Path' (Rel E2eTestsDir) (Dir SnapshotsDir)
snapshotsDirInE2eTests = [reldir|SnapshotTest/snapshots|]

getSnapshotsDir :: IO (Path' Abs (Dir SnapshotsDir))
getSnapshotsDir = getE2eTestsDir <&> (</> snapshotsDirInE2eTests)

snapshotDirInSnapshotsDir :: String -> SnapshotType -> Path' (Rel SnapshotsDir) (Dir SnapshotDir)
snapshotDirInSnapshotsDir snapshotTestName snapshotType = (fromJust . parseRelDir) (snapshotTestName ++ "-" ++ show snapshotType)

snapshotFileListManifestFileInSnapshotDir :: Path' (Rel SnapshotDir) (File SnapshotFileListManifestFile)
snapshotFileListManifestFileInSnapshotDir = [relfile|snapshot-file-list.manifest|]

-- | Inverse of 'gitRootFromSnapshotDir'.
-- NOTE: If you change this function, change the other one too.
snapshotDirInGitRootDir :: String -> SnapshotType -> Path' (Rel GitRootDir) (Dir SnapshotDir)
snapshotDirInGitRootDir snapshotTestName snapshotType =
  waspcDirInGitRootDir
    </> e2eTestsDirInWaspcDir
    </> snapshotsDirInE2eTests
    </> snapshotDirInSnapshotsDir snapshotTestName snapshotType

-- | Inverse of 'snapshotDirInGitRootDir'.
-- NOTE: If you change this function, change the other one too.
gitRootFromSnapshotDir :: Path' (Rel SnapshotDir) (Dir GitRootDir)
gitRootFromSnapshotDir = [reldir|../../../../../|]
