module Test.FileSystem
  ( TestCaseDir,
    getTestCaseDir,
    testsOutputsDirInTestsDir,
    getTestOutputsDir,
  )
where

import Data.Maybe (fromJust)
import FileSystem (E2eTestsDir, getE2eTestsDir)
import StrongPath (Abs, Dir, Path', Rel, parseRelDir, reldir, (</>))

-- | A directory inside the 'TestOutputsDir' where outputs of a specific e2e test case are stored.
data TestCaseDir

-- | The directory where all 'TestCaseDir' are created.
data TestOutputsDir

-- | Get the directory for a specific test case: test-outputs/{testName}/{testCaseName}
getTestCaseDir :: String -> String -> IO (Path' Abs (Dir TestCaseDir))
getTestCaseDir testName testCaseName = do
  e2eTestsDir <- getE2eTestsDir
  return $ e2eTestsDir </> testsOutputsDirInTestsDir </> testCaseDirInTestOutputsDir (testName ++ "/" ++ testCaseName)

getTestOutputsDir :: IO (Path' Abs (Dir TestOutputsDir))
getTestOutputsDir = (</> testsOutputsDirInTestsDir) <$> getE2eTestsDir

testCaseDirInTestOutputsDir :: String -> Path' (Rel TestOutputsDir) (Dir TestCaseDir)
testCaseDirInTestOutputsDir = fromJust . parseRelDir

testsOutputsDirInTestsDir :: Path' (Rel E2eTestsDir) (Dir TestOutputsDir)
testsOutputsDirInTestsDir = [reldir|Test/test-outputs|]
