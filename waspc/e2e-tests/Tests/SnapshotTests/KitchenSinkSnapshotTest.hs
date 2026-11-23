module Tests.SnapshotTests.KitchenSinkSnapshotTest (kitchenSinkSnapshotTest) where

import E2E
import Tests.SnapshotTests.SdkPackageExports (assertSdkPackageExportsExist)

kitchenSinkSnapshotTest :: E2ETest
kitchenSinkSnapshotTest =
  snapshotTest "kitchen-sink" $ do
    copyGitTrackedDir "examples/kitchen-sink" waspAppDirName
    inDirectory waspAppDirName $ do
      copyFile ".env.server.example" ".env.server"
      appendToFile ".env.server" "\nDATABASE_URL=mock-database-url"
      wasp ["install"]
      wasp ["compile"]
      assertSdkPackageExportsExist
