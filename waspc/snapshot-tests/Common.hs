module Common
  ( ProjectRoot,
    SnapshotTestsDir,
    GoldensDir,
    getSnapshotTestsOutputsDir
  )
where

import Control.Monad (unless)
import StrongPath (Abs, Dir, Path', Rel, reldir, (</>))
import qualified StrongPath as SP
import System.Directory (getCurrentDirectory)
import System.FilePath (takeFileName)

-- The "waspc" directory.
data ProjectRoot

data SnapshotTestsDir

data GoldensDir

getProjectRootPath :: IO (Path' Abs (Dir ProjectRoot))
getProjectRootPath = do
  -- NOTE: Cabal launches `cabal test` from root of the project, so this should always be some absolute path to waspc.
  absCwd <- getCurrentDirectory
  -- Just a little extra safeguard here since we are doing destructive file ops.
  unless (takeFileName absCwd == "waspc") (error "Expecting test process to be invoked from waspc dir")
  SP.parseAbsDir absCwd

snapshotTestsDirInProjectRoot :: Path' (Rel ProjectRoot) (Dir SnapshotTestsDir)
snapshotTestsDirInProjectRoot = [reldir|snapshot-tests|]

getSnapshotTestsDir :: IO (Path' Abs (Dir SnapshotTestsDir))
getSnapshotTestsDir = (</> snapshotTestsDirInProjectRoot) <$> getProjectRootPath

testOutputsDirInSnapshotTests :: Path' (Rel SnapshotTestsDir) (Dir GoldensDir)
testOutputsDirInSnapshotTests = [reldir|test-outputs|]

getSnapshotTestsOutputsDir :: IO (Path' Abs (Dir GoldensDir))
getSnapshotTestsOutputsDir = (</> testOutputsDirInSnapshotTests) <$> getSnapshotTestsDir
