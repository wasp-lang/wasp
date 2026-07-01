module Tests.SnapshotTests.KitchenSinkSnapshotTest (kitchenSinkSnapshotTest) where

import SnapshotTest (SnapshotTest, makeSnapshotTest)
import Steps
  ( appendToFile,
    copyContentsOfGitTrackedDirToSnapshotWaspProjectDir,
    copyFile,
    inWaspProjectDir,
    runCommand,
    waspCliCompile,
    waspCliInstall,
  )
import StrongPath (reldir)
import Tests.SdkPackageExportsTest (assertSdkPackageExports)

kitchenSinkSnapshotTest :: SnapshotTest
kitchenSinkSnapshotTest =
  makeSnapshotTest "kitchen-sink" $ do
    copyContentsOfGitTrackedDirToSnapshotWaspProjectDir [reldir|examples/kitchen-sink|]
    inWaspProjectDir $ do
      createDotEnvServerFile
      normalizePostgresConnectionString
      runCommand waspCliInstall
      runCommand waspCliCompile
      assertSdkPackageExports
  where
    createDotEnvServerFile = copyFile ".env.server.example" ".env.server"
    normalizePostgresConnectionString = appendToFile ".env.server" "\nDATABASE_URL=mock-database-url"
