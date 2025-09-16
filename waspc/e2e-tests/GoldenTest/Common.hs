module GoldenTest.Common
  ( GoldenTestSnapshotType (..),
    GoldenTestSnapshotDir,
    GoldenTestWaspAppDir,
    ExpectedFilesManifestFile,
    goldenTestSnapshotDirInTestOutputsDir,
    goldenTestWaspAppDirInGitRoot,
    gitRootInGoldenTestWaspAppDir,
    goldenTestWaspAppDirInGoldenTestSnapshotDir,
    expectedFilesManifestFileInGoldenTestSnapshotDir,
  )
where

import Common (GitRepositoryRoot, TestOutputsDir, e2eTestsDirInWaspcDir, testOutputsDirInE2eTests, waspcDirInGitRoot)
import Data.Maybe (fromJust)
import StrongPath (Dir, File, Path', Rel, parseRelDir, reldir, relfile, (</>))

data GoldenTestSnapshotType = Golden | Current

instance Show GoldenTestSnapshotType where
  show Golden = "golden"
  show Current = "current"

-- The directory inside a TestOutputsDir where a golden test snapshot is stored.
-- The snapshot is either the "golden" (expected) or "current" (actual) snapshot.
-- It follows a "<golden-test-name>-<snapshot-type>" naming convention.
-- e.g. "wasp-new-golden", "kitchen-sink-current"
data GoldenTestSnapshotDir

-- The directory inside a GoldenTestSnapshotDir where the Wasp app is.
-- It is named after the golden test, e.g., "kitchen-sink".
data GoldenTestWaspAppDir

data ExpectedFilesManifestFile

goldenTestSnapshotDirInTestOutputsDir :: String -> GoldenTestSnapshotType -> Path' (Rel TestOutputsDir) (Dir GoldenTestSnapshotDir)
goldenTestSnapshotDirInTestOutputsDir projectName snapshotType = (fromJust . parseRelDir) (projectName ++ "-" ++ show snapshotType)

expectedFilesManifestFileInGoldenTestSnapshotDir :: Path' (Rel GoldenTestSnapshotDir) (File ExpectedFilesManifestFile)
expectedFilesManifestFileInGoldenTestSnapshotDir = [relfile|expected-files.manifest|]

goldenTestWaspAppDirInGoldenTestSnapshotDir :: String -> Path' (Rel GoldenTestSnapshotDir) (Dir GoldenTestWaspAppDir)
goldenTestWaspAppDirInGoldenTestSnapshotDir = fromJust . parseRelDir

-- | Inverse of 'gitRootInGoldenTestWaspAppDir'.
--   If you change this function, change the other one too.
goldenTestWaspAppDirInGitRoot :: String -> GoldenTestSnapshotType -> Path' (Rel GitRepositoryRoot) (Dir GoldenTestWaspAppDir)
goldenTestWaspAppDirInGitRoot projectName snapshotType =
  waspcDirInGitRoot
    </> e2eTestsDirInWaspcDir
    </> testOutputsDirInE2eTests
    </> goldenTestSnapshotDirInTestOutputsDir projectName snapshotType
    </> goldenTestWaspAppDirInGoldenTestSnapshotDir projectName

-- | Inverse of 'goldenTestWaspAppDirInGitRoot'.
--   If you change this function, change the other one too.
gitRootInGoldenTestWaspAppDir :: Path' (Rel GoldenTestWaspAppDir) (Dir GitRepositoryRoot)
gitRootInGoldenTestWaspAppDir = [reldir|../../../../|]
