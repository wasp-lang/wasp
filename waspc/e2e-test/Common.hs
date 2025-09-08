module Common
  ( GitRepositoryRoot,
    HaskellProjectRoot,
    E2eTestDir,
    TestOutputsDir,
    getE2eTestDir,
    getTestOutputsDir,
    getProjectRootPath,
    gitRootFromGoldenTestProjectDir,
    goldenTestProjectDirInGitRoot,
  )
where

import Control.Monad (unless)
import Data.Maybe (fromJust)
import StrongPath (Abs, Dir, Path', Rel, parseRelDir, reldir, (</>))
import qualified StrongPath as SP
import System.Directory (getCurrentDirectory)
import System.FilePath (takeFileName)

-- The root of the git repository.
data GitRepositoryRoot

-- The "waspc" directory.
data HaskellProjectRoot

data E2eTestDir

data TestOutputsDir

-- The directory inside TestOutputsDir where golden test runs are stored.
-- It follows a "<project-name>-<golden-test-type>" naming convention.
-- e.g. "wasp-new-golden", "kitchen-sink-current"
data GoldenTestDir

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

e2eTestDirInProjectRoot :: Path' (Rel HaskellProjectRoot) (Dir E2eTestDir)
e2eTestDirInProjectRoot = [reldir|e2e-test|]

getE2eTestDir :: IO (Path' Abs (Dir E2eTestDir))
getE2eTestDir = (</> e2eTestDirInProjectRoot) <$> getProjectRootPath

testOutputsDirInE2eTest :: Path' (Rel E2eTestDir) (Dir TestOutputsDir)
testOutputsDirInE2eTest = [reldir|test-outputs|]

getTestOutputsDir :: IO (Path' Abs (Dir TestOutputsDir))
getTestOutputsDir = (</> testOutputsDirInE2eTest) <$> getE2eTestDir

goldenTestProjectDirInGitRoot :: String -> String -> Path' (Rel GitRepositoryRoot) (Dir GoldenTestProjectDir)
goldenTestProjectDirInGitRoot projectName goldenTestType =
  projectRootInGitRoot
    </> e2eTestDirInProjectRoot
    </> testOutputsDirInE2eTest
    </> parseGoldenTestDirInTestsOutputs
    </> parseGoldenTestProjectDirInGoldenTest
  where
    goldenTestDirName = projectName ++ "-" ++ goldenTestType
    parseGoldenTestDirInTestsOutputs :: Path' (Rel TestOutputsDir) (Dir GoldenTestDir)
    parseGoldenTestDirInTestsOutputs = fromJust (parseRelDir goldenTestDirName)
    parseGoldenTestProjectDirInGoldenTest :: Path' (Rel GoldenTestDir) (Dir GoldenTestProjectDir)
    parseGoldenTestProjectDirInGoldenTest = fromJust (parseRelDir projectName)

-- TODO: define from goldenTestProjectDirInGitRoot
gitRootFromGoldenTestProjectDir :: Path' (Rel GoldenTestProjectDir) (Dir GitRepositoryRoot)
gitRootFromGoldenTestProjectDir = [reldir|../../../../|]
