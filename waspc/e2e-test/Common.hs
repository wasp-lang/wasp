module Common
  ( ProjectRoot,
    E2eTestDir,
    GoldensDir,
    getE2eTestDir,
    getTestOutputsDir,
    getProjectRootPath,
  )
where

import Control.Monad (unless)
import StrongPath (Abs, Dir, Path', Rel, reldir, (</>))
import qualified StrongPath as SP
import System.Directory (getCurrentDirectory)
import System.FilePath (takeFileName)

-- The "waspc" directory.
data ProjectRoot

data E2eTestDir

data GoldensDir

getProjectRootPath :: IO (Path' Abs (Dir ProjectRoot))
getProjectRootPath = do
  -- NOTE: Stack/Cabal launches `stack test` from root of the project, so this should always be some absolute path to waspc.
  absCwd <- getCurrentDirectory
  -- Just a little extra safeguard here since we are doing destructive file ops.
  unless (takeFileName absCwd == "waspc") (error "Expecting test process to be invoked from waspc dir")
  SP.parseAbsDir absCwd

e2eTestDirInProjectRoot :: Path' (Rel ProjectRoot) (Dir E2eTestDir)
e2eTestDirInProjectRoot = [reldir|e2e-test|]

getE2eTestDir :: IO (Path' Abs (Dir E2eTestDir))
getE2eTestDir = (</> e2eTestDirInProjectRoot) <$> getProjectRootPath

testOutputsDirInE2eTest :: Path' (Rel E2eTestDir) (Dir GoldensDir)
testOutputsDirInE2eTest = [reldir|test-outputs|]

getTestOutputsDir :: IO (Path' Abs (Dir GoldensDir))
getTestOutputsDir = (</> testOutputsDirInE2eTest) <$> getE2eTestDir
