import Control.Concurrent (getNumCapabilities)
import Control.Monad (unless, when)
import E2E.Heartbeat (heartbeatReporter)
import E2E.RunConfig (RunConfig (..), getRunConfig, maxJobsEnvVar, skipDockerEnvVar, waspCliCmdEnvVar)
import E2E.Signals (stopOnFirstCtrlC)
import E2E.Test (E2ETest, checkOutputDirs, toTestTree)
import SnapshotTest (testTreeFromSnapshotTest)
import System.Environment (setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.Info (os)
import System.Process (readProcessWithExitCode)
import Test (testTreeFromTest)
import Test.Tasty (TestTree, defaultMainWithIngredients, localOption, testGroup)
import Test.Tasty.Ingredients (composeReporters)
import Test.Tasty.Runners (NumThreads (..), consoleTestReporter, listingTests)
import Tests.IncrementalTypeCheckingTest (incrementalTypeCheckingTest)
import Tests.SdkPackageExportsTest (makeSdkPackageExportsTestTree)
import Tests.ServerTypeCheckingTest (serverTypeCheckingTest)
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
import UnliftIO.Async (pooledMapConcurrentlyN)

main :: IO ()
main = do
  if os == "mingw32"
    then putStrLn "Skipping end-to-end tests on Windows due to tests using *nix-only commands"
    else do
      stopOnFirstCtrlC
      runConfig <- getRunConfig
      -- The tests that still use the shell commands DSL read the Wasp CLI and
      -- whether Docker is disabled from the environment.
      setEnv waspCliCmdEnvVar runConfig.waspCliPath
      if runConfig.isDockerEnabled then unsetEnv skipDockerEnvVar else setEnv skipDockerEnvVar "1"
      -- With --hide-successes, the skipped tests don't show up as skipped.
      unless runConfig.isDockerEnabled
        $ putStrLn
        $ "Docker is disabled via " ++ skipDockerEnvVar ++ ", so the tests that need it pass without running their Docker steps."
      checkOutputDirs (e2eSnapshotTests ++ e2eTests)
      -- Runs before tasty parses its options, so also for e.g. `--list-tests`.
      warmUpWaspCli runConfig.waspCliPath
      tests <- makeTestTree runConfig
      defaultMainWithIngredients
        [listingTests, composeReporters heartbeatReporter consoleTestReporter]
        (maybe id (localOption . NumThreads) runConfig.maxJobs tests)

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

-- TODO: Investigate automatically discovering the tests.
-- TODO: Port the remaining tests from the shell commands DSL to the 'E2E' one.
--       See: github.com/wasp-lang/wasp/issues/3404
makeTestTree :: RunConfig -> IO TestTree
makeTestTree runConfig = do
  maxConcurrentSnapshotBuilds <- getMaxConcurrentSnapshotBuilds runConfig
  putStrLn $ "Preparing snapshot tests with up to " ++ show maxConcurrentSnapshotBuilds ++ " concurrent build(s). Override with " ++ maxJobsEnvVar ++ "."
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
      [ testGroup "Snapshot Tests" (map (toTestTree runConfig) e2eSnapshotTests ++ snapshotTestTrees),
        testGroup "Shell tests" shellTestTrees,
        testGroup "Tests" (sdkPackageExportsTestTree : map (toTestTree runConfig) e2eTests)
      ]

-- | The tests written with the 'E2E' module.
e2eTests, e2eSnapshotTests :: [E2ETest]
e2eTests =
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
    incrementalTypeCheckingTest,
    serverTypeCheckingTest
  ]
e2eSnapshotTests = [waspNewSnapshotTest]

-- | How many snapshot tests we prepare concurrently.
--
-- Each snapshot test shells out to Node tooling (`npm install`, `tsc`,
-- `vite build`, ...) that already parallelizes across every core, so preparing
-- all of them at once oversubscribes the CPU. We therefore cap the fan-out.
--
-- The default scales with the number of available cores (a fraction of them,
-- since each build is itself multi-core). Set @WASP_E2E_TEST_MAX_JOBS@ to a
-- positive integer to override it (e.g. @1@ for fully serial preparation).
getMaxConcurrentSnapshotBuilds :: RunConfig -> IO Int
getMaxConcurrentSnapshotBuilds runConfig = do
  numCores <- getNumCapabilities
  return $ case runConfig.maxJobs of
    Just maxJobs -> maxJobs
    Nothing -> max 1 (numCores `div` 4)
