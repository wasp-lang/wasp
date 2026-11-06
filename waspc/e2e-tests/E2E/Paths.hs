module E2E.Paths
  ( getWaspcDir,
    getTestOutputsDir,
    getSnapshotsDir,
  )
where

import Control.Monad (unless)
import Data.Functor ((<&>))
import System.Directory (getCurrentDirectory)
import System.FilePath (takeFileName, (</>))

getWaspcDir :: IO FilePath
getWaspcDir = do
  -- NOTE: Cabal runs test suites from the package root, which is the waspc dir.
  cwd <- getCurrentDirectory
  -- A safeguard, since tests delete their output dirs.
  unless (takeFileName cwd == "waspc") (error "Expecting the e2e tests to be run from the waspc dir")
  return cwd

-- | Where tests run and leave their outputs, e.g. @<test-group>/<test-name>/@.
getTestOutputsDir :: IO FilePath
getTestOutputsDir = getWaspcDir <&> (</> "e2e-tests" </> "test-outputs")

-- | Contains the current and golden snapshot dirs, e.g. @wasp-new-current/@
-- and @wasp-new-golden/@.
getSnapshotsDir :: IO FilePath
getSnapshotsDir = getTestOutputsDir <&> (</> "snapshots")
