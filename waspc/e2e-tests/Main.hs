import Control.Monad (unless, when)
import E2E.Heartbeat (heartbeatReporter)
import E2E.RunConfig (RunConfig (..), getRunConfig, skipDockerEnvVar)
import E2E.Signals (stopOnFirstCtrlC)
import E2E.Test (E2ETest, checkOutputDirs, toTestTree)
import System.Exit (ExitCode (..))
import System.Info (os)
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, defaultMainWithIngredients, localOption, testGroup)
import Test.Tasty.Ingredients (composeReporters)
import Test.Tasty.Runners (NumThreads (..), consoleTestReporter, listingTests)
import Tests.IncrementalTypeCheckingTest (incrementalTypeCheckingTest)
import Tests.ServerTypeCheckingTest (serverTypeCheckingTest)
import Tests.SnapshotTests.KitchenSinkSnapshotTest (kitchenSinkSnapshotTest)
import Tests.SnapshotTests.WaspBuildSnapshotTest (waspBuildSnapshotTest)
import Tests.SnapshotTests.WaspCompileSnapshotTest (waspCompileSnapshotTest)
import Tests.SnapshotTests.WaspMigrateSnapshotTest (waspMigrateSnapshotTest)
import Tests.SnapshotTests.WaspNewSnapshotTest (waspNewSnapshotTest)
import Tests.ViteBuildTest (viteBuildTest)
import Tests.ViteConfigTest (viteConfigTest)
import Tests.VitestTest (vitestTest)
import Tests.WaspBuildStartTest (waspBuildStartTest)
import Tests.WaspBuildTest (waspBuildTest)
import Tests.WaspCleanTest (waspCleanTest)
import Tests.WaspCompileTest (waspCompileTest)
import Tests.WaspCompletionTest (waspCompletionTest)
import Tests.WaspDbMigrateDevTest (waspDbMigrateDevTest)
import Tests.WaspDbResetTest (waspDbResetTest)
import Tests.WaspDbSeedTest (waspDbSeedTest)
import Tests.WaspDbStartTest (waspDbStartTest)
import Tests.WaspDbStudioTest (waspDbStudioTest)
import Tests.WaspDepsTest (waspDepsTest)
import Tests.WaspDockerfileTest (waspDockerfileTest)
import Tests.WaspInstallTest (waspInstallTest)
import Tests.WaspNewTest (waspNewTest)
import Tests.WaspProjectLockTest (waspProjectLockTest)
import Tests.WaspShowTest (waspShowTest)
import Tests.WaspSpecAvailableTest (waspSpecAvailableTest)
import Tests.WaspSpecEntityTypesTest (waspSpecEntityTypesTest)
import Tests.WaspStartTest (waspStartTest)
import Tests.WaspStudioTest (waspStudioTest)
import Tests.WaspTelemetryTest (waspTelemetryTest)
import Tests.WaspTsSpecNodeEnvTest (waspTsSpecNodeEnvTest)
import Tests.WaspVersionTest (waspVersionTest)

main :: IO ()
main = do
  if os == "mingw32"
    then putStrLn "Skipping end-to-end tests on Windows due to tests using *nix-only commands"
    else do
      stopOnFirstCtrlC
      runConfig <- getRunConfig
      -- With --hide-successes, the skipped tests don't show up as skipped.
      unless runConfig.isDockerEnabled
        $ putStrLn
        $ "Docker is disabled via " ++ skipDockerEnvVar ++ ", so the tests that need it pass without running their Docker steps."
      checkOutputDirs (e2eSnapshotTests ++ e2eTests)
      -- Runs before tasty parses its options, so also for e.g. `--list-tests`.
      warmUpWaspCli runConfig.waspCliPath
      defaultMainWithIngredients
        [listingTests, composeReporters heartbeatReporter consoleTestReporter]
        (maybe id (localOption . NumThreads) runConfig.maxJobs $ makeTestTree runConfig)

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
makeTestTree :: RunConfig -> TestTree
makeTestTree runConfig =
  testGroup
    "E2E tests"
    [ testGroup "Snapshot Tests" (map (toTestTree runConfig) e2eSnapshotTests),
      testGroup "Tests" (map (toTestTree runConfig) e2eTests)
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
    waspBuildStartTest,
    waspTsSpecNodeEnvTest,
    viteBuildTest,
    viteConfigTest,
    vitestTest,
    waspCleanTest,
    waspProjectLockTest,
    waspSpecAvailableTest,
    waspShowTest,
    waspInstallTest,
    waspDepsTest,
    waspDockerfileTest,
    waspStudioTest,
    -- Wasp project db commands
    waspDbStartTest,
    waspDbStudioTest,
    waspDbSeedTest,
    waspDbResetTest,
    waspDbMigrateDevTest,
    waspSpecEntityTypesTest,
    incrementalTypeCheckingTest,
    serverTypeCheckingTest
  ]
e2eSnapshotTests =
  [ -- Tasty starts the tests in order, so the slowest ones go first.
    kitchenSinkSnapshotTest,
    waspBuildSnapshotTest,
    waspMigrateSnapshotTest,
    waspCompileSnapshotTest,
    waspNewSnapshotTest
  ]
