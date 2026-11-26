import Control.Monad (unless, when)
import E2E.Signals (stopOnFirstCtrlC)
import E2E.Test (checkOutputDirs, setTestSuiteEnv)
import E2E.TestSuiteEnv (TestSuiteEnv (..), getTestSuiteEnv, skipDockerEnvVar)
import qualified SnapshotTestsDiscoverDriver
import System.Exit (ExitCode (..))
import System.Info (os)
import System.Process (readProcessWithExitCode)
import Test.Tasty (defaultMain, localOption, testGroup)
import Test.Tasty.Runners (NumThreads (..), TestTree (TestGroup))
import qualified TestsDiscoverDriver

main :: IO ()
main = do
  if os == "mingw32"
    then putStrLn "Skipping end-to-end tests on Windows due to tests using *nix-only commands"
    else do
      stopOnFirstCtrlC
      testSuiteEnv <- getTestSuiteEnv
      -- With --hide-successes, the skipped tests don't show up as skipped.
      unless testSuiteEnv.isDockerEnabled
        $ putStrLn
        $ "Docker is disabled via " ++ skipDockerEnvVar ++ ", so the tests that need it pass without running, and snapshot tests skip their Docker steps."
      snapshotTests <- SnapshotTestsDiscoverDriver.tests
      tests <- TestsDiscoverDriver.tests
      let testTree =
            setTestSuiteEnv testSuiteEnv . maybe id (localOption . NumThreads) testSuiteEnv.maxJobs $
              testGroup
                "E2E tests"
                -- Tasty starts the tests in order, so the slowest ones go first.
                [renameGroup "Snapshot Tests" snapshotTests, renameGroup "Tests" tests]
      checkOutputDirs testTree
      -- Runs before tasty parses its options, so also for e.g. `--list-tests`.
      warmUpWaspCli testSuiteEnv.waspCliPath
      defaultMain testTree

-- | Builds the dev Wasp CLI once, serially, before the tests start invoking it
-- concurrently.
--
-- The dev CLI runs through `cabal run`, and Cabal does not support several
-- concurrent invocations sharing a single `dist-newstyle`: if the first build
-- isn't finished, the parallel invocations race to register the inplace package
-- and fail with "package.conf.inplace already exists". Doing one serial
-- invocation here forces that build to complete first, so the concurrent
-- invocations only ever run the already-built CLI.
warmUpWaspCli :: FilePath -> IO ()
warmUpWaspCli waspCliPath = do
  (exitCode, out, err) <- readProcessWithExitCode waspCliPath ["version"] ""
  when (exitCode /= ExitSuccess) $ fail $ "Running `" ++ waspCliPath ++ " version` failed:\n" ++ out ++ err

-- | tasty-discover names the group of the tests it collects after the driver
-- file, e.g. @e2e-tests/src/TestsDiscoverDriver.hs@.
renameGroup :: String -> TestTree -> TestTree
renameGroup name = \case
  TestGroup _ tests -> testGroup name tests
  testTree -> testGroup name [testTree]
