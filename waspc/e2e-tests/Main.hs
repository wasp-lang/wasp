import SnapshotTest (runSnapshotTest)
import SnapshotTest.KitchenSinkSnapshotTest (kitchenSinkSnapshotTest)
import SnapshotTest.WaspBuildSnapshotTest (waspBuildSnapshotTest)
import SnapshotTest.WaspCompileSnapshotTest (waspCompileSnapshotTest)
import SnapshotTest.WaspMigrateSnapshotTest (waspMigrateSnapshotTest)
import SnapshotTest.WaspNewSnapshotTest (waspNewSnapshotTest)
import System.Info (os)
import Test (runTest)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.WaspBuildTest (waspBuildTest)
import Test.WaspCleanTest (waspCleanTest)
import Test.WaspCompileTest (waspCompileTest)
import Test.WaspCompletionTest (waspCompletionTest)
import Test.WaspDbMigrateDevTest (waspDbMigrateDevTest)
import Test.WaspDbResetTest (waspDbResetTest)
import Test.WaspDbSeedTest (waspDbSeedTest)
import Test.WaspDepsTest (waspDepsTest)
import Test.WaspDockerfileTest (waspDockerfileTest)
import Test.WaspInfoTest (waspInfoTest)
import Test.WaspNewTest
  ( waspNewBasicInteractiveTest,
    waspNewBasicTest,
    waspNewMinimalInteractiveTest,
    waspNewMinimalTest,
    waspNewSaasInteractiveTest,
    waspNewSaasTest,
  )
import Test.WaspTelemetryTest (waspTelemetryTest)
import Test.WaspVersionTest (waspVersionTest)

main :: IO ()
main = do
  if os == "mingw32"
    then putStrLn "Skipping end-to-end tests on Windows due to tests using *nix-only commands"
    else e2eTests >>= defaultMain

-- TODO: Investigate automatically discovering the tests.
-- TODO: Refactor tests DSL so it does not depend on bash commands. Use pure Haskell instead.
e2eTests :: IO TestTree
e2eTests = do
  snapshotTests <-
    mapM
      runSnapshotTest
      [ waspNewSnapshotTest,
        waspCompileSnapshotTest,
        waspBuildSnapshotTest,
        waspMigrateSnapshotTest,
        kitchenSinkSnapshotTest
      ]
  tests <-
    mapM
      runTest
      [ -- general Wasp commads
        waspNewMinimalTest,
        waspNewMinimalInteractiveTest,
        waspNewBasicTest,
        waspNewBasicInteractiveTest,
        waspNewSaasTest,
        waspNewSaasInteractiveTest,
        waspTelemetryTest,
        waspCompletionTest,
        waspVersionTest,
        -- Wasp project commands
        waspCompileTest,
        -- NOTE(Franjo): The following tests have the `FIXME` comment because they
        -- are long running processes, i.e. can run for infinite amount of time.
        -- While test cases for these CLI commands are written, because I dind't
        -- find a reliable way to test long running processes yet, I've commented
        -- them out. If you have an idea on how to make them works, do let me know.
        -- FIXME: waspStartTest,
        waspBuildTest,
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
      [ testGroup "Snapshot Tests" snapshotTests,
        testGroup "Tests" tests
      ]
