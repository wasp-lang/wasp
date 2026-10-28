module Tests (allTests) where

import E2e.Test (E2eTest, testGroup)
import Tests.IncrementalTypeCheckingTest (incrementalTypeCheckingTests)
import Tests.SnapshotTests.KitchenSinkSnapshotTest (kitchenSinkSnapshotTest)
import Tests.SnapshotTests.WaspBuildSnapshotTest (waspBuildSnapshotTest)
import Tests.SnapshotTests.WaspCompileSnapshotTest (waspCompileSnapshotTest)
import Tests.SnapshotTests.WaspMigrateSnapshotTest (waspMigrateSnapshotTest)
import Tests.SnapshotTests.WaspNewSnapshotTest (waspNewSnapshotTest)
import Tests.ViteBuildTest (viteBuildTests)
import Tests.ViteConfigTest (viteConfigTests)
import Tests.WaspBuildStartTest (waspBuildStartTests)
import Tests.WaspBuildTest (waspBuildTests)
import Tests.WaspCleanTest (waspCleanTests)
import Tests.WaspCompileTest (waspCompileTests)
import Tests.WaspCompletionTest (waspCompletionTests)
import Tests.WaspDbMigrateDevTest (waspDbMigrateDevTests)
import Tests.WaspDbResetTest (waspDbResetTests)
import Tests.WaspDbSeedTest (waspDbSeedTests)
import Tests.WaspDbStartTest (waspDbStartTests)
import Tests.WaspDbStudioTest (waspDbStudioTests)
import Tests.WaspDepsTest (waspDepsTests)
import Tests.WaspDockerfileTest (waspDockerfileTests)
import Tests.WaspInstallTest (waspInstallTests)
import Tests.WaspNewTest (waspNewTests)
import Tests.WaspProjectLockTest (waspProjectLockTests)
import Tests.WaspShowTest (waspShowTests)
import Tests.WaspSpecAvailableTest (waspSpecAvailableTests)
import Tests.WaspSpecEntityTypesTest (waspSpecEntityTypesTests)
import Tests.WaspStartTest (waspStartTests)
import Tests.WaspStudioTest (waspStudioTests)
import Tests.WaspTelemetryTest (waspTelemetryTests)
import Tests.WaspTsSpecNodeEnvTest (waspTsSpecNodeEnvTests)
import Tests.WaspVersionTest (waspVersionTests)

-- | Tests start in this order (as far as resources allow), so the slowest ones come first.
allTests :: [E2eTest]
allTests =
  testGroup
    "snapshot"
    [ kitchenSinkSnapshotTest,
      waspBuildSnapshotTest,
      waspMigrateSnapshotTest,
      waspCompileSnapshotTest,
      waspNewSnapshotTest
    ]
    ++ concat
      [ waspBuildStartTests,
        waspStartTests,
        waspDbStartTests,
        waspStudioTests,
        waspDbStudioTests,
        waspProjectLockTests,
        viteBuildTests,
        waspSpecAvailableTests,
        waspBuildTests,
        waspTsSpecNodeEnvTests,
        waspDbResetTests,
        waspDbSeedTests,
        waspDbMigrateDevTests,
        incrementalTypeCheckingTests,
        waspCompileTests,
        waspSpecEntityTypesTests,
        viteConfigTests,
        waspInstallTests,
        waspCleanTests,
        waspShowTests,
        waspDepsTests,
        waspDockerfileTests,
        waspNewTests,
        waspTelemetryTests,
        waspCompletionTests,
        waspVersionTests
      ]
