module Common
  ( GoldenTestType (..),
    GitRepositoryRoot,
    E2eTestsDir,
    TestOutputsDir,
    getTestOutputsDir,
    goldenTestDirInTestOutputsDir,
    gitRootFromGoldenTestProjectDir,
    expectedFilesManifestFileInGoldenTestDir,
  )
where

import Control.Monad (unless)
import Data.Maybe (fromJust)
import StrongPath (Abs, Dir, File, Path', Rel, parseRelDir, reldir, relfile, (</>))
import qualified StrongPath as SP
import System.Directory (getCurrentDirectory)
import System.FilePath (takeFileName)

-- The root of the git repository.
data GitRepositoryRoot

-- The "waspc" directory.
data HaskellProjectRoot

data E2eTestsDir

data TestOutputsDir

data GoldenTestType = Golden | Current

instance Show GoldenTestType where
  show Golden = "golden"
  show Current = "current"

-- The directory inside TestOutputsDir where golden test runs are stored.
-- It follows a "<project-name>-<golden-test-type>" naming convention.
-- e.g. "wasp-new-golden", "kitchen-sink-current"
data GoldenTestDir

data ExpectedFilesManifest

-- The directory inside a golden test run directory where the project for that test is located.
-- It is named after the project, e.g., "kitchen-sink".
data GoldenTestProjectDir

projectRootInGitRoot :: Path' (Rel GitRepositoryRoot) (Dir HaskellProjectRoot)
projectRootInGitRoot = [reldir|waspc|]

getProjectRootPath :: IO (Path' Abs (Dir HaskellProjectRoot))
getProjectRootPath = do
  -- NOTE: Cabal launches `cabal test` from root of the project, so this should always be some absolute path to waspc.
  absCwd <- getCurrentDirectory
  -- Just a little extra safeguard here since we are doing destructive file ops.
  unless (takeFileName absCwd == "waspc") (error "Expecting test process to be invoked from waspc dir")
  SP.parseAbsDir absCwd

e2eTestsDirInProjectRoot :: Path' (Rel HaskellProjectRoot) (Dir E2eTestsDir)
e2eTestsDirInProjectRoot = [reldir|e2e-tests|]

getE2eTestsDir :: IO (Path' Abs (Dir E2eTestsDir))
getE2eTestsDir = (</> e2eTestsDirInProjectRoot) <$> getProjectRootPath

testOutputsDirInE2eTests :: Path' (Rel E2eTestsDir) (Dir TestOutputsDir)
testOutputsDirInE2eTests = [reldir|test-outputs|]

getTestOutputsDir :: IO (Path' Abs (Dir TestOutputsDir))
getTestOutputsDir = (</> testOutputsDirInE2eTests) <$> getE2eTestsDir

goldenTestDirInTestOutputsDir :: String -> GoldenTestType -> Path' (Rel TestOutputsDir) (Dir GoldenTestDir)
goldenTestDirInTestOutputsDir projectName goldenTestType = (fromJust . parseRelDir) (projectName ++ "-" ++ show goldenTestType)

expectedFilesManifestFileInGoldenTestDir :: Path' (Rel GoldenTestDir) (File ExpectedFilesManifest)
expectedFilesManifestFileInGoldenTestDir = [relfile|expected-files.manifest|]

goldenTestProjectDirInGoldenTestDir :: String -> Path' (Rel GoldenTestDir) (Dir GoldenTestProjectDir)
goldenTestProjectDirInGoldenTestDir projectName = (fromJust . parseRelDir) projectName

goldenTestProjectDirInGitRoot :: String -> GoldenTestType -> Path' (Rel GitRepositoryRoot) (Dir GoldenTestProjectDir)
goldenTestProjectDirInGitRoot projectName goldenTestType =
  projectRootInGitRoot
    </> e2eTestsDirInProjectRoot
    </> testOutputsDirInE2eTests
    </> goldenTestDirInTestOutputsDir projectName goldenTestType
    </> goldenTestProjectDirInGoldenTestDir projectName

-- TODO: define from goldenTestProjectDirInGitRoot
gitRootFromGoldenTestProjectDir :: Path' (Rel GoldenTestProjectDir) (Dir GitRepositoryRoot)
gitRootFromGoldenTestProjectDir = [reldir|../../../../|]
