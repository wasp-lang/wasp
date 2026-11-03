import Control.Concurrent (getNumCapabilities)
import Control.Monad (mfilter, when)
import E2E.Heartbeat (heartbeatReporter)
import E2E.Test (toTestTree)
import SnapshotTest (testTreeFromSnapshotTest)
import System.Directory (findExecutable)
import System.Environment (lookupEnv, setEnv)
import System.Exit (ExitCode (..))
import System.Info (os)
import System.Process (readProcessWithExitCode)
import Test (testTreeFromTest)
import Test.Tasty (TestTree, defaultMainWithIngredients, localOption, testGroup)
import Test.Tasty.Ingredients (composeReporters)
import Test.Tasty.Runners (NumThreads (..), consoleTestReporter, listingTests)
import Tests.IncrementalTypeCheckingTest (incrementalTypeCheckingTest)
import Tests.SdkPackageExportsTest (makeSdkPackageExportsTestTree)
import Tests.SnapshotTests.KitchenSinkSnapshotTest (kitchenSinkSnapshotTest)
import Tests.SnapshotTests.WaspBuildSnapshotTest (waspBuildSnapshotTest)
import Tests.SnapshotTests.WaspCompileSnapshotTest (waspCompileSnapshotTest)
import Tests.SnapshotTests.WaspMigrateSnapshotTest (waspMigrateSnapshotTest)
import Tests.SnapshotTests.WaspNewSnapshotTest (waspNewSnapshotTest)
import Tests.ViteBuildTest (viteBuildTest)
import Tests.ViteConfigTest (viteConfigTest)
import Tests.VitestTest (vitestTest)
import Tests.WaspBuildTest (waspBuildTest)
import Tests.WaspCleanTest (waspCleanTest)
import Tests.WaspCompileTest (waspCompileTest)
import Tests.WaspCompletionTest (waspCompletionTest)
import Tests.WaspDbMigrateDevTest (waspDbMigrateDevTest)
import Tests.WaspDbResetTest (waspDbResetTest)
import Tests.WaspDbSeedTest (waspDbSeedTest)
import Tests.WaspDbStartTest (waspDbStartTest)
import Tests.WaspDepsTest (waspDepsTest)
import Tests.WaspDockerfileTest (waspDockerfileTest)
import Tests.WaspInstallTest (waspInstallTest)
import Tests.WaspNewTest (waspNewTest)
import Tests.WaspProjectLockTest (waspProjectLockTest)
import Tests.WaspShowTest (waspShowTest)
import Tests.WaspSpecAvailableTest (waspSpecAvailableTest)
import Tests.WaspSpecEntityTypesTest (waspSpecEntityTypesTest)
import Tests.WaspStartTest (waspStartTest)
import Tests.WaspTelemetryTest (waspTelemetryTest)
import Tests.WaspTsSpecNodeEnvTest (waspTsSpecNodeEnvTest)
import Tests.WaspVersionTest (waspVersionTest)
import Text.Read (readMaybe)
import UnliftIO.Async (pooledMapConcurrentlyN)

main :: IO ()
main = do
  if os == "mingw32"
    then putStrLn "Skipping end-to-end tests on Windows due to tests using *nix-only commands"
    else do
      setWaspCliCmd
      warmUpWaspCli
      maxJobs <- (>>= readMaybe) <$> lookupEnv "WASP_E2E_TEST_MAX_JOBS" :: IO (Maybe Int)
      tests <- e2eTests
      defaultMainWithIngredients
        [listingTests, composeReporters heartbeatReporter consoleTestReporter]
        (maybe id (localOption . NumThreads) (mfilter (>= 1) maxJobs) tests)

-- | The tests run the Wasp CLI from @WASP_CLI_CMD@ (e.g. @tools/wasp-cli-dev@
-- from @./run@). When it's not set, we use the @wasp-cli@ that Cabal builds
-- and puts on the PATH for this test suite.
setWaspCliCmd :: IO ()
setWaspCliCmd =
  lookupEnv "WASP_CLI_CMD" >>= \case
    Just waspCliCmd | not (null waspCliCmd) -> return ()
    _ ->
      findExecutable "wasp-cli"
        >>= maybe (fail "Couldn't find the wasp-cli executable. Set WASP_CLI_CMD.") (setEnv "WASP_CLI_CMD")

-- | Builds the dev Wasp CLI once, serially, before the tests start invoking it
-- concurrently.
--
-- The dev CLI runs through `cabal run`, and Cabal does not support several
-- concurrent invocations sharing a single `dist-newstyle`: if the first build
-- isn't finished, the parallel invocations race to register the inplace package
-- and fail with "package.conf.inplace already exists". Doing one serial
-- invocation here forces that build to complete first, so the concurrent
-- invocations only ever run the already-built CLI.
warmUpWaspCli :: IO ()
warmUpWaspCli = do
  waspCliCmd <- maybe (fail "WASP_CLI_CMD is not set.") return =<< lookupEnv "WASP_CLI_CMD"
  (exitCode, _, err) <- readProcessWithExitCode waspCliCmd ["version"] ""
  when (exitCode /= ExitSuccess) $ fail $ "Running `" ++ waspCliCmd ++ " version` failed:\n" ++ err

-- TODO: Investigate automatically discovering the tests.
-- TODO: Port the remaining tests from the shell commands DSL to the 'E2E' one.
--       See: github.com/wasp-lang/wasp/issues/3404
e2eTests :: IO TestTree
e2eTests = do
  maxConcurrentSnapshotBuilds <- getMaxConcurrentSnapshotBuilds
  putStrLn $ "Preparing snapshot tests with up to " ++ show maxConcurrentSnapshotBuilds ++ " concurrent build(s). Override with WASP_E2E_TEST_MAX_JOBS."
  snapshotTestTrees <-
    pooledMapConcurrentlyN
      maxConcurrentSnapshotBuilds
      testTreeFromSnapshotTest
      [ waspCompileSnapshotTest,
        waspBuildSnapshotTest,
        waspMigrateSnapshotTest,
        kitchenSinkSnapshotTest
      ]
  shellTestTrees <-
    mapM
      testTreeFromTest
      [ -- FIXME: waspBuildStartTest,
        waspProjectLockTest
        -- FIXME: waspStudioTest,
        -- FIXME: waspDbStudioTest,
      ]
  sdkPackageExportsTestTree <- makeSdkPackageExportsTestTree

  return $
    testGroup
      "E2E tests"
      [ testGroup "Snapshot Tests" (toTestTree waspNewSnapshotTest : snapshotTestTrees),
        testGroup "Shell tests" shellTestTrees,
        testGroup
          "Tests"
          ( sdkPackageExportsTestTree
              : map
                toTestTree
                [ -- general Wasp commands
                  waspNewTest,
                  waspTelemetryTest,
                  waspCompletionTest,
                  waspVersionTest,
                  -- Wasp project commands
                  waspCompileTest,
                  waspStartTest,
                  waspBuildTest,
                  waspTsSpecNodeEnvTest,
                  viteBuildTest,
                  viteConfigTest,
                  vitestTest,
                  waspCleanTest,
                  waspSpecAvailableTest,
                  waspShowTest,
                  waspInstallTest,
                  waspDepsTest,
                  waspDockerfileTest,
                  -- Wasp project db commands
                  waspDbStartTest,
                  waspDbSeedTest,
                  waspDbResetTest,
                  waspDbMigrateDevTest,
                  waspSpecEntityTypesTest,
                  incrementalTypeCheckingTest
                ]
          )
      ]

-- | How many snapshot tests we prepare concurrently.
--
-- Each snapshot test shells out to Node tooling (`npm install`, `tsc`,
-- `vite build`, ...) that already parallelizes across every core, so preparing
-- all of them at once oversubscribes the CPU. We therefore cap the fan-out.
--
-- The default scales with the number of available cores (a fraction of them,
-- since each build is itself multi-core). Set @WASP_E2E_TEST_MAX_JOBS@ to a
-- positive integer to override it (e.g. @1@ for fully serial preparation).
getMaxConcurrentSnapshotBuilds :: IO Int
getMaxConcurrentSnapshotBuilds = do
  numCores <- getNumCapabilities
  override <- (>>= readMaybe) <$> lookupEnv "WASP_E2E_TEST_MAX_JOBS"
  return $ case override of
    Just n | n >= 1 -> n
    _ -> max 1 (numCores `div` 4)
