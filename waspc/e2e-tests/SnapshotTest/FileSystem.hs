module SnapshotTest.FileSystem
  ( SnapshotType (..),
    SnapshotsDir,
    SnapshotDir,
    SnapshotWaspProjectDir,
    SnapshotFileListManifestFile,
    SnapshotFile,
    asWaspProjectDir,
    snapshotsDirInE2eTests,
    getSnapshotsDir,
    snapshotDirInSnapshotsDir,
    snapshotDirInGitRootDir,
    gitRootFromSnapshotDir,
    snapshotWaspProjectDirInSnapshotDir,
    snapshotFileListManifestFileInSnapshotDir,
  )
where

import Data.Maybe (fromJust)
import FileSystem (E2eTestsDir, GitRootDir, e2eTestsDirInWaspcDir, getE2eTestsDir, waspcDirInGitRootDir)
import SnapshotTest.Snapshot (SnapshotType (..))
import StrongPath (Dir, File, Path, Path', Rel, castDir, parseRelDir, reldir, relfile, (</>))
import StrongPath.Types (Abs)
import Wasp.Project.Common (WaspProjectDir)

-- | The directory where all snapshots are stored.
data SnapshotsDir

-- | The directory inside a 'SnapshotsDir' where a snapshot is stored.
-- The snapshot is either a "golden" (expected) or a "current" (actual) snapshot.
-- It follows a @<snapshot-test-name>-<snapshot-type>@ naming convention.
-- e.g. @testName-golden@, @kitchen-sink-current@
data SnapshotDir

-- | Represent any file inside of a 'SnapshotDir'.
data SnapshotFile

-- | The Wasp app directory inside of a 'SnapshotDir'.
data SnapshotWaspProjectDir

-- | Converts a 'SnapshotWaspProjectDir' to a 'Wasp.Project.Common.WaspProjectDir'.
-- This is safe because every snapshot Wasp project directory is also a Wasp project directory.
asWaspProjectDir :: Path s a (Dir SnapshotWaspProjectDir) -> Path s a (Dir WaspProjectDir)
asWaspProjectDir = castDir

-- | The file inside of a 'SnapshotDir'.
-- Lists all files that should exist in the 'SnapshotDir' directory.
data SnapshotFileListManifestFile

snapshotsDirInE2eTests :: Path' (Rel E2eTestsDir) (Dir SnapshotsDir)
snapshotsDirInE2eTests = [reldir|snapshots|]

getSnapshotsDir :: IO (Path' Abs (Dir SnapshotsDir))
getSnapshotsDir = (</> snapshotsDirInE2eTests) <$> getE2eTestsDir

snapshotDirInSnapshotsDir :: String -> SnapshotType -> Path' (Rel SnapshotsDir) (Dir SnapshotDir)
snapshotDirInSnapshotsDir snapshotTestName snapshotType = (fromJust . parseRelDir) (snapshotTestName ++ "-" ++ show snapshotType)

snapshotFileListManifestFileInSnapshotDir :: Path' (Rel SnapshotDir) (File SnapshotFileListManifestFile)
snapshotFileListManifestFileInSnapshotDir = [relfile|snapshot-file-list.manifest|]

snapshotWaspProjectDirInSnapshotDir :: String -> Path' (Rel SnapshotDir) (Dir SnapshotWaspProjectDir)
snapshotWaspProjectDirInSnapshotDir snapshotWaspProjectName = fromJust . parseRelDir $ snapshotWaspProjectName

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
gitRootFromSnapshotDir = [reldir|../../../../|]
