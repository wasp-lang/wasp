-- | The whole run of the e2e tests: what all of its tests share, and what the
-- run checks about them before they start.
--
-- The tests are plain tasty 'TestTree's, so that tasty-discover can find them
-- (see @TestsDiscoverDriver.hs@). They get the 'TestSuiteEnv' from tasty's
-- options, which the e2e tests' main sets (see 'setTestSuiteEnv').
-- 'E2E.Steps.TestEnv' has what's specific to each test.
module E2E.Suite
  ( TestSuiteEnv (..),
    getTestSuiteEnv,
    skipDockerEnvVar,
    setTestSuiteEnv,
    withTestSuiteEnv,
    claimOutputDir,
    checkOutputDirs,
  )
where

import Control.Concurrent.MVar (MVar, newMVar)
import Control.Monad (mfilter, unless)
import Data.List (intercalate, isPrefixOf, nub)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tagged (Tagged (..))
import System.Directory (getCurrentDirectory, makeAbsolute)
import System.Environment (lookupEnv)
import System.FilePath (isPathSeparator, joinPath, takeDirectory, takeFileName, (</>))
import Test.Tasty (TestTree, askOption, localOption)
import Test.Tasty.Options (IsOption (..), lookupOption)
import Test.Tasty.Runners (TreeFold (..), foldTestTree, trivialFold)
import Text.Read (readMaybe)

data TestSuiteEnv = TestSuiteEnv
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
getTestSuiteEnv :: IO TestSuiteEnv
getTestSuiteEnv = do
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
    TestSuiteEnv
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

-- | Set for the whole test tree by the e2e tests' main.
newtype TestSuiteEnvOption = TestSuiteEnvOption TestSuiteEnv

instance IsOption TestSuiteEnvOption where
  defaultValue = error "The e2e tests need a test suite env. Run them with the e2e tests' main, which sets it."
  parseValue = const Nothing
  optionName = Tagged "e2e-test-suite-env"
  optionHelp = Tagged "What all the e2e tests of the run share. Not a command line option."

setTestSuiteEnv :: TestSuiteEnv -> TestTree -> TestTree
setTestSuiteEnv = localOption . TestSuiteEnvOption

withTestSuiteEnv :: (TestSuiteEnv -> TestTree) -> TestTree
withTestSuiteEnv makeTestTree = askOption $ \(TestSuiteEnvOption testSuiteEnv) -> makeTestTree testSuiteEnv

-- | The dir a test runs in, relative to the test outputs dir, as the names of
-- the dirs on the way, e.g. @["wasp-start", "stop-on-sigint"]@. Tests delete
-- their dirs, so no two tests may share one (see 'checkOutputDirs').
newtype OutputDir = OutputDir (Maybe [String])

instance IsOption OutputDir where
  defaultValue = OutputDir Nothing
  parseValue = const Nothing
  optionName = Tagged "e2e-output-dir"
  optionHelp = Tagged "The dir an e2e test runs in. Not a command line option."

-- | Declares the dir the test runs in, so that the run can check that no
-- other test uses it.
claimOutputDir :: [String] -> TestTree -> TestTree
claimOutputDir = localOption . OutputDir . Just

-- | Fails if a test or group name isn't a single dir name (e.g. it's empty or
-- has a slash), so that a test could run in another's dir or outside of the
-- test outputs dir, or if two tests would run in the same dir, or one in
-- another's dir.
--
-- Checks all of the tests, also the ones a pattern leaves out.
checkOutputDirs :: TestTree -> IO ()
checkOutputDirs testTree = do
  unless (null invalidNames)
    $ fail
    $ "Some e2e test or group names aren't a single directory name, e.g. they're empty or have a slash: " ++ intercalate ", " (map show invalidNames)
  unless (null clashingDirs)
    $ fail
    $ "Some e2e tests would run in the same directory, so their names must change: " ++ intercalate ", " clashingDirs
  where
    dirs = foldTestTree trivialFold {foldSingle = \options _ _ -> [dir | OutputDir (Just dir) <- [lookupOption options]]} mempty testTree
    invalidNames = nub [name | dir <- dirs, name <- dir, not (isSingleDirName name)]
    isSingleDirName name = name `notElem` ["", ".", ".."] && not (any isPathSeparator name)
    clashingDirs =
      nub
        [ joinPath dir
        | (index, dir) <- zip [0 :: Int ..] dirs,
          (otherIndex, otherDir) <- zip [0 ..] dirs,
          index /= otherIndex,
          dir `isPrefixOf` otherDir
        ]
