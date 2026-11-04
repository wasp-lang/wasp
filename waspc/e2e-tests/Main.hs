import Control.Monad (mfilter, when)
import E2E.Heartbeat (heartbeatReporter)
import E2E.Test (toTestTree)
import System.Directory (findExecutable)
import System.Environment (lookupEnv, setEnv)
import System.Exit (ExitCode (..))
import System.Info (os)
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, defaultMainWithIngredients, localOption, testGroup)
import Test.Tasty.Ingredients (composeReporters)
import Test.Tasty.Runners (NumThreads (..), consoleTestReporter, listingTests)
import Tests.IncrementalTypeCheckingTest (incrementalTypeCheckingTest)
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
import Text.Read (readMaybe)

main :: IO ()
main = do
  if os == "mingw32"
    then putStrLn "Skipping end-to-end tests on Windows due to tests using *nix-only commands"
    else do
      setWaspCliCmd
      warmUpWaspCli
      maxJobs <- (>>= readMaybe) <$> lookupEnv "WASP_E2E_TEST_MAX_JOBS" :: IO (Maybe Int)
      defaultMainWithIngredients
        [listingTests, composeReporters heartbeatReporter consoleTestReporter]
        (maybe id (localOption . NumThreads) (mfilter (>= 1) maxJobs) e2eTests)

-- | The tests run the Wasp CLI from @WASP_CLI_CMD@ (e.g. @tools/wasp-cli-dev@
-- from the mise tasks). When it's not set, we use the @wasp-cli@ that Cabal builds
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
e2eTests :: TestTree
e2eTests =
  testGroup
    "E2E tests"
    [ -- Tasty starts the tests in order, so the slowest ones go first.
      testGroup "Snapshot Tests" $
        map
          toTestTree
          [ kitchenSinkSnapshotTest,
            waspBuildSnapshotTest,
            waspMigrateSnapshotTest,
            waspCompileSnapshotTest,
            waspNewSnapshotTest
          ],
      testGroup "Tests" $
        map
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
            incrementalTypeCheckingTest
          ]
    ]
