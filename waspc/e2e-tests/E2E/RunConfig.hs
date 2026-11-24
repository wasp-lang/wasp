-- | Settings of a test run. We read them from the environment once, when the
-- run starts, and pass them to the tests.
module E2E.RunConfig
  ( RunConfig (..),
    getRunConfig,
    skipDockerEnvVar,
  )
where

import Control.Concurrent.MVar (MVar, newMVar)
import Control.Monad (mfilter, unless)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Directory (getCurrentDirectory, makeAbsolute)
import System.Environment (lookupEnv)
import System.FilePath (isPathSeparator, takeDirectory, takeFileName, (</>))
import Text.Read (readMaybe)

data RunConfig = RunConfig
  { -- | The Wasp CLI under test: an absolute path, or a program name to look up
    -- in @PATH@.
    waspCliPath :: FilePath,
    gitRootDir :: FilePath,
    -- | Where tests run and leave their outputs, e.g. @<test-group>/<test-name>/@.
    testOutputsDir :: FilePath,
    isDockerEnabled :: Bool,
    -- | How many tests run at once. 'Nothing' leaves it to tasty.
    maxJobs :: Maybe Int,
    -- | The ports 'E2E.Ports' gave out to the tests of this run.
    allocatedPorts :: MVar (Set Int)
  }

-- | Must be run from the waspc dir (see 'getWaspcDir').
getRunConfig :: IO RunConfig
getRunConfig = do
  waspcDir <- getWaspcDir
  waspCliPath <-
    lookupNonEmptyEnv waspCliCmdEnvVar >>= \case
      -- Runs the current state of the waspc source.
      Nothing -> return $ waspcDir </> "tools" </> "wasp-cli-dev"
      -- Tests run in their own dirs, so a relative path must be made absolute
      -- while we're still in the waspc dir.
      Just waspCliCmd
        | any isPathSeparator waspCliCmd -> makeAbsolute waspCliCmd
        | otherwise -> return waspCliCmd
  isDockerEnabled <- lookupNonEmptyEnv skipDockerEnvVar >>= parseIsDockerEnabled
  maxJobs <- lookupNonEmptyEnv maxJobsEnvVar >>= traverse parseMaxJobs
  allocatedPorts <- newMVar Set.empty
  return
    RunConfig
      { waspCliPath,
        gitRootDir = takeDirectory waspcDir,
        testOutputsDir = waspcDir </> "e2e-tests" </> "test-outputs",
        isDockerEnabled,
        maxJobs,
        allocatedPorts
      }
  where
    parseIsDockerEnabled = \case
      Nothing -> return True
      Just "0" -> return True
      Just "1" -> return False
      Just value -> fail $ skipDockerEnvVar ++ " must be 1 (skip) or 0 (don't skip), but it is " ++ show value ++ "."

    parseMaxJobs value = case readMaybe value of
      Just jobs | jobs >= 1 -> return jobs
      _ -> fail $ maxJobsEnvVar ++ " must be a positive integer, but it is " ++ show value ++ "."

getWaspcDir :: IO FilePath
getWaspcDir = do
  -- NOTE: Cabal runs test suites from the package root, which is the waspc dir.
  cwd <- getCurrentDirectory
  -- A safeguard, since tests delete their output dirs.
  unless (takeFileName cwd == "waspc") (error "Expecting the e2e tests to be run from the waspc dir")
  return cwd

-- | Empty variables count as unset.
lookupNonEmptyEnv :: String -> IO (Maybe String)
lookupNonEmptyEnv name = mfilter (not . null) <$> lookupEnv name

-- | A single executable, run without a shell, so it can't contain arguments.
waspCliCmdEnvVar :: String
waspCliCmdEnvVar = "WASP_CLI_CMD"

skipDockerEnvVar :: String
skipDockerEnvVar = "WASP_E2E_TESTS_SKIP_DOCKER"

maxJobsEnvVar :: String
maxJobsEnvVar = "WASP_E2E_TEST_MAX_JOBS"
