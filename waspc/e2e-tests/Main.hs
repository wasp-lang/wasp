import EphemeralTest (runEphemeralTest)
import EphemeralTest.WaspCompletionEphemeralTest (waspCompletionEphemeralTest)
import EphemeralTest.WaspDepsEphemeralTest (waspDepsEphemeralTest)
import EphemeralTest.WaspDockerfileEphemeralTest (waspDockerfileEphemeralTest)
import EphemeralTest.WaspInfoEphemeralTest (waspInfoEphemeralTest)
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
import EphemeralTest.WaspDbResetEphemeralTest (waspDbResetEphemeralTest)
import EphemeralTest.WaspDbSeedEphemeralTest (waspDbSeedEphemeralTest)
import EphemeralTest.WaspNewEphemeralTest (waspNewMinimalEphemeralTest, waspNewInteractiveMinimalEphemeralTest, waspNewBasicEphemeralTest, waspNewInteractiveBasicEphemeralTest, waspNewSaasEphemeralTest, waspNewInteractiveSaasEphemeralTest)
import EphemeralTest.WaspBuildEphemeralTest (waspBuildEphemeralTest)
import EphemeralTest.WaspCleanEphemeralTest (waspCleanEphemeralTest)
import EphemeralTest.WaspCompileEphemeralTest (waspCompileEphemeralTest)
import EphemeralTest.WaspDbMigrateDevEphemeralTest (waspDbMigrateDevEphemeralTest)

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
        waspNewInteractiveMinimalEphemeralTest,
        waspNewBasicEphemeralTest, 
        waspNewInteractiveBasicEphemeralTest, 
        waspNewSaasEphemeralTest, 
        waspNewInteractiveSaasEphemeralTest,
        waspTelemetryEphemeralTest,
        waspCompletionEphemeralTest,
        waspVersionEphemeralTest,
        -- Wasp project commands
        waspCompileEphemeralTest,
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
      [ 
        testGroup "Snapshot Tests" snapshotTests,
        testGroup "Ephemeral Tests" ephemeralTests
      ]
