import Control.Concurrent (getNumCapabilities)
import FileSystem (getWaspcDirPath, waspCliDevToolInWaspcDir)
import SnapshotTest (testTreeFromSnapshotTest)
import StrongPath ((</>))
import qualified StrongPath as SP
import System.Environment (lookupEnv, setEnv)
import System.Info (os)
import System.Process (callCommand)
import Test (testTreeFromTest)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Tests.SdkPackageExportsTest (makeSdkPackageExportsTestTree)
import Tests.SnapshotTests.WaspBuildSnapshotTest (waspBuildSnapshotTest)
import Tests.SnapshotTests.WaspCompileSnapshotTest (waspCompileSnapshotTest)
import Tests.SnapshotTests.WaspMigrateSnapshotTest (waspMigrateSnapshotTest)
import Tests.SnapshotTests.WaspNewSnapshotTest (waspNewSnapshotTest)
import Tests.ViteBuildTest (viteBuildTest)
import Tests.ViteConfigTest (viteConfigTest)
import Tests.WaspBuildTest (waspBuildTest)
import Tests.WaspCleanTest (waspCleanTest)
import Tests.WaspCompileTest (waspCompileTest)
import Tests.WaspCompletionTest (waspCompletionTest)
import Tests.WaspDbMigrateDevTest (waspDbMigrateDevTest)
import Tests.WaspDbResetTest (waspDbResetTest)
import Tests.WaspDbSeedTest (waspDbSeedTest)
import Tests.WaspDepsTest (waspDepsTest)
import Tests.WaspDockerfileTest (waspDockerfileTest)
import Tests.WaspInfoTest (waspInfoTest)
import Tests.WaspInstallTest (waspInstallTest)
import Tests.WaspNewTest (waspNewTest)
import Tests.WaspSpecAvailableTest (waspSpecAvailableTest)
import Tests.WaspSpecEntityTypesTest (waspSpecEntityTypesTest)
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
      ensureE2eTestsEnvironment
      warmUpWaspCli
      e2eTests >>= defaultMain

ensureE2eTestsEnvironment :: IO ()
ensureE2eTestsEnvironment = do
  existing <- lookupEnv "WASP_CLI_CMD"
  case existing of
    Just _ -> return ()
    Nothing -> do
      -- Runs the tests using the current state of the `waspc` project.
      waspcDir <- getWaspcDirPath
      let devWaspCliCmd = SP.fromAbsFile (waspcDir </> waspCliDevToolInWaspcDir)
      setEnv "WASP_CLI_CMD" devWaspCliCmd

-- | Builds the dev Wasp CLI once, serially, before the snapshot tests start
-- invoking it concurrently (via 'pooledMapConcurrentlyN' in 'e2eTests').
--
-- The dev CLI runs through `cabal run`, and Cabal does not support several
-- concurrent invocations sharing a single `dist-newstyle`: if the first build
-- isn't finished, the parallel invocations race to register the inplace package
-- and fail with "package.conf.inplace already exists". Doing one serial
-- invocation here forces that build to complete first, so the concurrent
-- invocations only ever run the already-built CLI.
warmUpWaspCli :: IO ()
warmUpWaspCli = callCommand "$WASP_CLI_CMD version 2>&1 >/dev/null" -- We don't need any output.

-- TODO: Investigate automatically discovering the tests.
-- TODO: Refactor tests DSL so it does not depend on bash commands. Use pure Haskell instead.
--       See: github.com/wasp-lang/wasp/issues/3404
e2eTests :: IO TestTree
e2eTests = do
  maxConcurrentSnapshotBuilds <- getMaxConcurrentSnapshotBuilds
  putStrLn $ "Preparing snapshot tests with up to " ++ show maxConcurrentSnapshotBuilds ++ " concurrent build(s). Override with WASP_E2E_TEST_MAX_JOBS."
  snapshotTestTrees <-
    pooledMapConcurrentlyN
      maxConcurrentSnapshotBuilds
      testTreeFromSnapshotTest
      [ waspNewSnapshotTest,
        waspCompileSnapshotTest,
        waspBuildSnapshotTest,
        waspMigrateSnapshotTest
      ]
  shellTestTrees <-
    mapM
      testTreeFromTest
      [ -- general Wasp commads
        waspNewTest,
        waspTelemetryTest,
        waspCompletionTest,
        waspVersionTest,
        -- Wasp project commands
        waspCompileTest,
        -- NOTE(Franjo): The following tests have the `FIXME` comment because they
        -- are long running processes, which we haven't implmemented support for yet.
        -- These will be fixed as part of the refactor to pure Haskell tests.
        -- FIXME: waspStartTest,
        waspBuildTest,
        waspTsSpecNodeEnvTest,
        viteBuildTest,
        viteConfigTest,
        -- FIXME: waspBuildStartTest,
        waspCleanTest,
        waspSpecAvailableTest,
        waspInfoTest,
        waspInstallTest,
        waspDepsTest,
        waspDockerfileTest,
        -- FIXME: waspStudioTest,
        -- Wasp project db commands
        -- FIXME: waspDbStartTest,
        -- FIXME: waspDbStudioTest,
        waspDbSeedTest,
        waspDbResetTest,
        waspDbMigrateDevTest,
        waspSpecEntityTypesTest
      ]
  sdkPackageExportsTestTree <- makeSdkPackageExportsTestTree

  return $
    testGroup
      "E2E tests"
      [ testGroup "Snapshot Tests" snapshotTestTrees,
        testGroup "Shell tests" shellTestTrees,
        testGroup "Tests" [sdkPackageExportsTestTree]
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
