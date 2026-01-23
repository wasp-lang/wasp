module Test.FileSystem
  ( TestDir,
    testDirInE2eTestsDir,
    getTestDir,
    TestWaspProjectDir,
    asWaspProjectDir,
    testWaspProjectDirInTestDir,
    testsTempDirInTestsDir,
    getTestsTempDir,
  )
where

import Data.Maybe (fromJust)
import FileSystem (E2eTestsDir, getE2eTestsDir)
import StrongPath (Abs, Dir, Path, Path', Rel, castDir, parseRelDir, reldir, (</>))
import Wasp.Project.Common (WaspProjectDir)

-- | The directory where all e2e tests' artifacts are stored.
data TestsTempDir

-- | The directory inside a 'TestsTempDir' where artifacts for a specific e2e test are stored.
-- It is named after the e2e test name.
data TestDir

-- | The Wasp app directory inside of a 'TestDir'.
data TestWaspProjectDir

-- | Converts a 'TestWaspProjectDir' to a 'Wasp.Project.Common.WaspProjectDir'.
-- This is safe because every e2e Wasp project directory is also a Wasp project directory.
asWaspProjectDir :: Path s a (Dir TestWaspProjectDir) -> Path s a (Dir WaspProjectDir)
asWaspProjectDir = castDir

getTestsTempDir :: IO (Path' Abs (Dir TestsTempDir))
getTestsTempDir = (</> testsTempDirInTestsDir) <$> getE2eTestsDir

getTestDir :: String -> IO (Path' Abs (Dir TestDir))
getTestDir testName = (</> testDirInE2eTestsDir testName) <$> getE2eTestsDir

testDirInE2eTestsDir :: String -> Path' (Rel E2eTestsDir) (Dir TestDir)
testDirInE2eTestsDir testName = testsTempDirInTestsDir </> (fromJust . parseRelDir $ testName)

testsTempDirInTestsDir :: Path' (Rel E2eTestsDir) (Dir TestsTempDir)
testsTempDirInTestsDir = [reldir|Test/temp|]

testWaspProjectDirInTestDir :: String -> Path' (Rel TestDir) (Dir TestWaspProjectDir)
testWaspProjectDirInTestDir testWaspProjectName = fromJust . parseRelDir $ testWaspProjectName
