import Control.Concurrent.Async (mapConcurrently)
import FileSystem (getWaspcDirPath, waspCliDevToolInWaspcDir)
import SnapshotTest (testTreeFromSnapshotTest)
import StrongPath ((</>))
import qualified StrongPath as SP
import System.Environment (lookupEnv, setEnv)
import System.Info (os)
import Test (testTreeFromTest)
import Test.Tasty (TestTree, defaultMain, testGroup)  
import Tests.SnapshotTests.KitchenSinkSnapshotTest (kitchenSinkSnapshotTest)
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

main :: IO ()
main = do
  if os == "mingw32"
    then putStrLn "Skipping end-to-end tests on Windows due to tests using *nix-only commands"
    else do
      ensureE2eTestsEnvironment
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

-- TODO: Investigate automatically discovering the tests.
-- TODO: Refactor tests DSL so it does not depend on bash commands. Use pure Haskell instead.
--       See: github.com/wasp-lang/wasp/issues/3404
e2eTests :: IO TestTree
e2eTests = do
  snapshotTestTrees <-
    mapConcurrently
      testTreeFromSnapshotTest
      [ waspNewSnapshotTest,
        waspCompileSnapshotTest,
        waspBuildSnapshotTest,
        waspMigrateSnapshotTest,
        kitchenSinkSnapshotTest
      ]
  testTrees <-
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

  return $
    testGroup
      "E2E tests"
      [ testGroup "Snapshot Tests" snapshotTestTrees,
        testGroup "Tests" testTrees
      ]
