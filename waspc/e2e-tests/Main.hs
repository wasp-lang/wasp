import EphemeralTest (runEphemeralTest)
import EphemeralTest.WaspBuildEphemeralTest (waspBuildEphemeralTest)
import EphemeralTest.WaspCleanEphemeralTest (waspCleanEphemeralTest)
import EphemeralTest.WaspCompileEphemeralTest (waspCompileEphemeralTest)
import EphemeralTest.WaspCompletionEphemeralTest (waspCompletionEphemeralTest)
import EphemeralTest.WaspDbMigrateDevEphemeralTest (waspDbMigrateDevEphemeralTest)
import EphemeralTest.WaspDbResetEphemeralTest (waspDbResetEphemeralTest)
import EphemeralTest.WaspDbSeedEphemeralTest (waspDbSeedEphemeralTest)
import EphemeralTest.WaspDepsEphemeralTest (waspDepsEphemeralTest)
import EphemeralTest.WaspDockerfileEphemeralTest (waspDockerfileEphemeralTest)
import EphemeralTest.WaspInfoEphemeralTest (waspInfoEphemeralTest)
import EphemeralTest.WaspNewEphemeralTest (waspNewBasicEphemeralTest, waspNewBasicInteractiveEphemeralTest, waspNewMinimalEphemeralTest, waspNewMinimalInteractiveEphemeralTest, waspNewSaasEphemeralTest, waspNewSaasInteractiveEphemeralTest)
import EphemeralTest.WaspTelemetryEphemeralTest (waspTelemetryEphemeralTest)
import EphemeralTest.WaspVersionEphemeralTest (waspVersionEphemeralTest)
import SnapshotTest (runSnapshotTest)
import SnapshotTest.KitchenSinkSnapshotTest (kitchenSinkSnapshotTest)
import SnapshotTest.WaspBuildSnapshotTest (waspBuildSnapshotTest)
import SnapshotTest.WaspCompileSnapshotTest (waspCompileSnapshotTest)
import SnapshotTest.WaspMigrateSnapshotTest (waspMigrateSnapshotTest)
import SnapshotTest.WaspNewSnapshotTest (waspNewSnapshotTest)
import System.Info (os)
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = do
  if os == "mingw32"
    then putStrLn "Skipping end-to-end tests on Windows due to tests using *nix-only commands"
    else tests >>= defaultMain

-- TODO: Investigate automatically discovering the tests.
tests :: IO TestTree
tests = do
  snapshotTests <-
    mapM
      runSnapshotTest
      [ waspNewSnapshotTest,
        waspCompileSnapshotTest,
        waspBuildSnapshotTest,
        waspMigrateSnapshotTest,
        kitchenSinkSnapshotTest
      ]
  ephemeralTests <-
    mapM
      runEphemeralTest
      [ -- general Wasp commads
        waspNewMinimalEphemeralTest,
        waspNewMinimalInteractiveEphemeralTest,
        waspNewBasicEphemeralTest,
        waspNewBasicInteractiveEphemeralTest,
        waspNewSaasEphemeralTest,
        waspNewSaasInteractiveEphemeralTest,
        waspTelemetryEphemeralTest,
        waspCompletionEphemeralTest,
        waspVersionEphemeralTest,
        -- Wasp project commands
        waspCompileEphemeralTest,
        -- NOTE(Franjo): The following tests have the `FIXME` comment because they
        -- are long running processes, i.e. can run for infinite amount of time.
        -- While test cases for these CLI commands are written, because I dind't
        -- find a reliable way to test long running processes yet, I've commented
        -- them out. If you have an idea on how to make them works, do let me know.
        -- FIXME: waspStartEphemeralTest,
        waspBuildEphemeralTest,
        -- FIXME: waspBuildStartEphemeralTest,
        waspCleanEphemeralTest,
        waspInfoEphemeralTest,
        waspDepsEphemeralTest,
        waspDockerfileEphemeralTest,
        -- FIXME: waspStudioEphemeralTest,
        -- Wasp project db commands
        -- FIXME: waspDbStartEphemeralTest,
        -- FIXME: waspDbStudioEphemeralTest,
        waspDbSeedEphemeralTest,
        waspDbResetEphemeralTest,
        waspDbMigrateDevEphemeralTest
      ]

  return $
    testGroup
      "E2E tests"
      [ testGroup "Snapshot Tests" snapshotTests,
        testGroup "Ephemeral Tests" ephemeralTests
      ]
