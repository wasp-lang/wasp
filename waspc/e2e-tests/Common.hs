module Common
  ( ProjectRoot,
    E2eTestsDir,
    GoldensDir,
    getTestOutputsDir,
  )
where

import Control.Monad (unless)
import StrongPath (Abs, Dir, Path', Rel, reldir, (</>))
import qualified StrongPath as SP
import System.Directory (getCurrentDirectory)
import System.FilePath (takeFileName)

-- The "waspc" directory.
data ProjectRoot

data E2eTestsDir

data GoldensDir

getProjectRootPath :: IO (Path' Abs (Dir ProjectRoot))
getProjectRootPath = do
  -- NOTE: Cabal launches `cabal test` from root of the project, so this should always be some absolute path to waspc.
  absCwd <- getCurrentDirectory
  -- Just a little extra safeguard here since we are doing destructive file ops.
  unless (takeFileName absCwd == "waspc") (error "Expecting test process to be invoked from waspc dir")
  SP.parseAbsDir absCwd

e2eTestDirInProjectRoot :: Path' (Rel ProjectRoot) (Dir E2eTestsDir)
e2eTestDirInProjectRoot = [reldir|e2e-tests|]

getE2eTestDir :: IO (Path' Abs (Dir E2eTestsDir))
getE2eTestDir = (</> e2eTestDirInProjectRoot) <$> getProjectRootPath

testOutputsDirInE2eTest :: Path' (Rel E2eTestsDir) (Dir GoldensDir)
testOutputsDirInE2eTest = [reldir|test-outputs|]

getTestOutputsDir :: IO (Path' Abs (Dir GoldensDir))
getTestOutputsDir = (</> testOutputsDirInE2eTest) <$> getE2eTestDir
