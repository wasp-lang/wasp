-- | Where the tests run.
module E2E.Paths
  ( getWaspcDir,
  )
where

import Control.Monad (unless)
import System.Directory (getCurrentDirectory)
import System.FilePath (takeFileName)

getWaspcDir :: IO FilePath
getWaspcDir = do
  -- NOTE: Cabal runs test suites from the package root, which is the waspc dir.
  cwd <- getCurrentDirectory
  -- A safeguard, since tests delete their output dirs.
  unless (takeFileName cwd == "waspc") (error "Expecting the e2e tests to be run from the waspc dir")
  return cwd
