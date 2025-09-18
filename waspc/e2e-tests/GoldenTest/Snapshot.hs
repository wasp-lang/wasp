module GoldenTest.Snapshot
  ( SnapshotType (..),
    SnapshotsDir,
    SnapshotDir,
    SnapshotWaspAppDir,
    SnapshotFileListManifestFile,
    snapshotsDirInE2eTests,
    getSnapshotsDir,
    snapshotDirInSnapshotsDir,
    snapshotWaspAppDirInGitRoot,
    gitRootInSnapshotWaspAppDir,
    snapshotWaspAppDirInSnapshotDir,
    snapshotFileListManifestFileInSnapshotDir,
  )
where

import Common (E2eTestsDir, GitRepositoryRoot, e2eTestsDirInWaspcDir, getE2eTestsDir, waspcDirInGitRoot)
import Data.Maybe (fromJust)
import StrongPath (Dir, File, Path', Rel, parseRelDir, reldir, relfile, (</>))
import StrongPath.Types (Abs)

data SnapshotsDir

-- | The directory inside a 'Common.SnapshotsDir' where a golden test snapshot is stored.
-- The snapshot is either a "golden" (expected) or a "current" (actual) snapshot.
-- It follows a @"<golden-test-name>-<snapshot-type>"@ naming convention.
-- e.g. @"testName-golden"@, @"kitchen-sink-current"@
data SnapshotDir

-- | The Wasp app directory inside of a 'SnapshotDir'.
-- TODO: centralni koncept wasp appa
-- It is named after the golden test.
-- e.g., @"testName"@, @"kitchen-sink"@.
data SnapshotWaspAppDir

-- | asdas
data SnapshotFileListManifestFile

data SnapshotType = Golden | Current

instance Show SnapshotType where
  show Golden = "golden"
  show Current = "current"

snapshotsDirInE2eTests :: Path' (Rel E2eTestsDir) (Dir SnapshotsDir)
snapshotsDirInE2eTests = [reldir|snapshots|]

getSnapshotsDir :: IO (Path' Abs (Dir SnapshotsDir))
getSnapshotsDir = (</> snapshotsDirInE2eTests) <$> getE2eTestsDir

snapshotDirInSnapshotsDir :: String -> SnapshotType -> Path' (Rel SnapshotsDir) (Dir SnapshotDir)
snapshotDirInSnapshotsDir projectName snapshotType = (fromJust . parseRelDir) (projectName ++ "-" ++ show snapshotType)

snapshotFileListManifestFileInSnapshotDir :: Path' (Rel SnapshotDir) (File SnapshotFileListManifestFile)
snapshotFileListManifestFileInSnapshotDir = [relfile|snapshot-file-list.manifest|]

snapshotWaspAppDirInSnapshotDir :: String -> Path' (Rel SnapshotDir) (Dir SnapshotWaspAppDir)
snapshotWaspAppDirInSnapshotDir = fromJust . parseRelDir

-- | Inverse of 'gitRootInSnapshotWaspAppDir'.
-- NOTE: If you change this function, change the other one too.
snapshotWaspAppDirInGitRoot :: String -> SnapshotType -> Path' (Rel GitRepositoryRoot) (Dir SnapshotWaspAppDir)
snapshotWaspAppDirInGitRoot projectName snapshotType =
  waspcDirInGitRoot
    </> e2eTestsDirInWaspcDir
    </> snapshotsDirInE2eTests
    </> snapshotDirInSnapshotsDir projectName snapshotType
    </> snapshotWaspAppDirInSnapshotDir projectName

-- | Inverse of 'snapshotWaspAppDirInGitRoot'.
-- NOTE: If you change this function, change the other one too.
gitRootInSnapshotWaspAppDir :: Path' (Rel SnapshotWaspAppDir) (Dir GitRepositoryRoot)
gitRootInSnapshotWaspAppDir = [reldir|../../../../|]
