module Common
  ( GitRepositoryRoot,
    WaspcDir,
    E2eTestsDir,
    TestOutputsDir,
    waspcDirInGitRoot,
    getWaspcDirPath,
    e2eTestsDirInWaspcDir,
    getE2eTestsDir,
    testOutputsDirInE2eTests,
    getTestOutputsDir,
  )
where

import Control.Monad (unless)
import StrongPath (Abs, Dir, Path', Rel, reldir, (</>))
import qualified StrongPath as SP
import System.Directory (getCurrentDirectory)
import System.FilePath (takeFileName)

data GitRepositoryRoot

data WaspcDir

data E2eTestsDir

data TestOutputsDir

waspcDirInGitRoot :: Path' (Rel GitRepositoryRoot) (Dir WaspcDir)
waspcDirInGitRoot = [reldir|waspc|]

getWaspcDirPath :: IO (Path' Abs (Dir WaspcDir))
getWaspcDirPath = do
  -- NOTE: Cabal launches `cabal test` from root of the project, so this should always be some absolute path to waspc.
  absCwd <- getCurrentDirectory
  -- Just a little extra safeguard here since we are doing destructive file ops.
  unless (takeFileName absCwd == "waspc") (error "Expecting test process to be invoked from waspc dir")
  SP.parseAbsDir absCwd

e2eTestsDirInWaspcDir :: Path' (Rel WaspcDir) (Dir E2eTestsDir)
e2eTestsDirInWaspcDir = [reldir|e2e-tests|]

getE2eTestsDir :: IO (Path' Abs (Dir E2eTestsDir))
getE2eTestsDir = (</> e2eTestsDirInWaspcDir) <$> getWaspcDirPath

testOutputsDirInE2eTests :: Path' (Rel E2eTestsDir) (Dir TestOutputsDir)
testOutputsDirInE2eTests = [reldir|test-outputs|]

getTestOutputsDir :: IO (Path' Abs (Dir TestOutputsDir))
getTestOutputsDir = (</> testOutputsDirInE2eTests) <$> getE2eTestsDir
