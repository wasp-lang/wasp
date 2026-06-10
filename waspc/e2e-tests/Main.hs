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
    else defaultMain e2eTests

-- TODO: Investigate automatically discovering the tests.
e2eTests :: TestTree
e2eTests =
  testGroup
    "E2E tests"
    [ testGroup
        "Snapshot Tests"
        ( map
            testTreeFromSnapshotTest
            [ waspNewSnapshotTest,
              waspCompileSnapshotTest,
              waspBuildSnapshotTest,
              waspMigrateSnapshotTest,
              kitchenSinkSnapshotTest
            ]
        ),
      testGroup
        "Tests"
        ( map
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
              -- Adding support should now be a matter of building on 'Command.startCommand'
              -- (e.g. a step that starts a command, waits for some output, and terminates it).
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
        )
    ]
