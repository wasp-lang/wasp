module Test.FileSystem
  ( TestDir,
    getTestDir,
    testsOutputsDirInTestsDir,
    getTestOutputsDir,
  )
where

import Data.Maybe (fromJust)
import FileSystem (E2eTestsDir, getE2eTestsDir)
import StrongPath (Abs, Dir, Path', Rel, parseRelDir, reldir, (</>))

-- | A directory inside the 'TestOutputsDir' where outputs of a specific e2e test are stored.
data TestDir

-- | The directory where all 'TestDir' are created.
data TestOutputsDir

getTestDir :: String -> IO (Path' Abs (Dir TestDir))
getTestDir testName = (</> (testsOutputsDirInTestsDir </> testDirInTestOutputsDir testName)) <$> getE2eTestsDir

getTestOutputsDir :: IO (Path' Abs (Dir TestOutputsDir))
getTestOutputsDir = (</> testsOutputsDirInTestsDir) <$> getE2eTestsDir

testDirInTestOutputsDir :: String -> Path' (Rel TestOutputsDir) (Dir TestDir)
testDirInTestOutputsDir = fromJust . parseRelDir

testsOutputsDirInTestsDir :: Path' (Rel E2eTestsDir) (Dir TestOutputsDir)
testsOutputsDirInTestsDir = [reldir|Test/test-outputs|]
