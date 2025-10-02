module FileSystem
  ( GitRootDir,
    WaspcDir,
    E2eTestsDir,
    waspcDirInGitRootDir,
    getWaspcDirPath,
    e2eTestsDirInWaspcDir,
    getE2eTestsDir,
  )
where

import Control.Monad (unless)
import StrongPath (Abs, Dir, Path', Rel, reldir, (</>))
import qualified StrongPath as SP
import System.Directory (getCurrentDirectory)
import System.FilePath (takeFileName)

data GitRootDir

data WaspcDir

data E2eTestsDir

waspcDirInGitRootDir :: Path' (Rel GitRootDir) (Dir WaspcDir)
waspcDirInGitRootDir = [reldir|waspc|]

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
