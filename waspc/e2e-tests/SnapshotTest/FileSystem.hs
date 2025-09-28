module SnapshotTest.FileSystem
  ( SnapshotType (..),
    SnapshotsDir,
    SnapshotDir,
    SnapshotWaspAppDir,
    SnapshotFileListManifestFile,
    SnapshotFile,
    asWaspAppDir,
    asSnapshotFile,
    snapshotsDirInE2eTests,
    getSnapshotsDir,
    snapshotDirInSnapshotsDir,
    snapshotWaspAppDirInRepoRoot,
    gitRootInSnapshotWaspAppDir,
    snapshotWaspAppDirInSnapshotDir,
    snapshotFileListManifestFileInSnapshotDir,
  )
where

import Data.Maybe (fromJust)
import FileSystem (E2eTestsDir, GitRepositoryRoot, e2eTestsDirInWaspcDir, getE2eTestsDir, waspcDirInGitRoot)
import SnapshotTest.Snapshot (SnapshotType (..))
import StrongPath (Dir, File, Path, Path', Rel, castDir, castFile, parseRelDir, reldir, relfile, (</>))
import StrongPath.Types (Abs)
import WaspApp.FileSystem (WaspAppDir)

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
-- We hardcode its name to @wasp-app@ so that the snapshots directory is more readable.
data SnapshotWaspAppDir

-- | Converts a 'SnapshotWaspAppDir' to a 'WaspApp.FileSystem.WaspAppDir'.
-- This is safe because every snapshot Wasp app directory is also a Wasp app directory.
asWaspAppDir :: Path s a (Dir SnapshotWaspAppDir) -> Path s a (Dir WaspAppDir)
asWaspAppDir = castDir

-- | The file inside of a 'SnapshotDir'.
-- Lists all files that should exist in the 'SnapshotDir' directory.
data SnapshotFileListManifestFile

-- | Converts a 'SnapshotFileListManifestFile' to a 'SnapshotFile'.
-- This is safe because every snapshot file list manifest file is also a snapshot file.
asSnapshotFile :: Path s a (File SnapshotFileListManifestFile) -> Path s a (File SnapshotFile)
asSnapshotFile = castFile

snapshotsDirInE2eTests :: Path' (Rel E2eTestsDir) (Dir SnapshotsDir)
snapshotsDirInE2eTests = [reldir|snapshots|]

getSnapshotsDir :: IO (Path' Abs (Dir SnapshotsDir))
getSnapshotsDir = (</> snapshotsDirInE2eTests) <$> getE2eTestsDir

snapshotDirInSnapshotsDir :: String -> SnapshotType -> Path' (Rel SnapshotsDir) (Dir SnapshotDir)
snapshotDirInSnapshotsDir snapshotTestName snapshotType = (fromJust . parseRelDir) (snapshotTestName ++ "-" ++ show snapshotType)

snapshotFileListManifestFileInSnapshotDir :: Path' (Rel SnapshotDir) (File SnapshotFileListManifestFile)
snapshotFileListManifestFileInSnapshotDir = [relfile|snapshot-file-list.manifest|]

snapshotWaspAppDirInSnapshotDir :: String -> Path' (Rel SnapshotDir) (Dir SnapshotWaspAppDir)
snapshotWaspAppDirInSnapshotDir snapshotWaspAppName = fromJust . parseRelDir $ snapshotWaspAppName

-- | Inverse of 'gitRootInSnapshotWaspAppDir'.
-- NOTE: If you change this function, change the other one too.
snapshotWaspAppDirInRepoRoot :: String -> SnapshotType -> String -> Path' (Rel GitRepositoryRoot) (Dir SnapshotWaspAppDir)
snapshotWaspAppDirInRepoRoot snapshotTestName snapshotType snapshotWaspAppName =
  waspcDirInGitRoot
    </> e2eTestsDirInWaspcDir
    </> snapshotsDirInE2eTests
    </> snapshotDirInSnapshotsDir snapshotTestName snapshotType
    </> snapshotWaspAppDirInSnapshotDir snapshotWaspAppName

-- | Inverse of 'snapshotWaspAppDirInRepoRoot'.
-- NOTE: If you change this function, change the other one too.
gitRootInSnapshotWaspAppDir :: Path' (Rel SnapshotWaspAppDir) (Dir GitRepositoryRoot)
gitRootInSnapshotWaspAppDir = [reldir|../../../../|]
