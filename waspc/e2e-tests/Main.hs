import Control.Concurrent.Async (mapConcurrently)
import SnapshotTest (testTreeFromSnapshotTest)
import System.Info (os)
import Test (testTreeFromTest)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Tests.SnapshotTests.KitchenSinkSnapshotTest (kitchenSinkSnapshotTest)
import Tests.SnapshotTests.WaspBuildSnapshotTest (waspBuildSnapshotTest)
import Tests.SnapshotTests.WaspCompileSnapshotTest (waspCompileSnapshotTest)
import Tests.SnapshotTests.WaspMigrateSnapshotTest (waspMigrateSnapshotTest)
import Tests.SnapshotTests.WaspNewSnapshotTest (waspNewSnapshotTest)
import Tests.ViteBuildTest (viteBuildTest)
import Tests.ViteOptionsValidationTest (viteOptionsValidationTest)
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
import Tests.WaspNewTest (waspNewTest)
import Tests.WaspTelemetryTest (waspTelemetryTest)
import Tests.WaspVersionTest (waspVersionTest)

main :: IO ()
main = do
  if os == "mingw32"
    then putStrLn "Skipping end-to-end tests on Windows due to tests using *nix-only commands"
    else e2eTests >>= defaultMain

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
        viteBuildTest,
        viteOptionsValidationTest,
        -- FIXME: waspBuildStartTest,
        waspCleanTest,
        waspInfoTest,
        waspDepsTest,
        waspDockerfileTest,
        -- FIXME: waspStudioTest,
        -- Wasp project db commands
        -- FIXME: waspDbStartTest,
        -- FIXME: waspDbStudioTest,
        waspDbSeedTest,
        waspDbResetTest,
        waspDbMigrateDevTest
      ]

  return $
    testGroup
      "E2E tests"
      [ testGroup "Snapshot Tests" snapshotTestTrees,
        testGroup "Tests" testTrees
      ]
