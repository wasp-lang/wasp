module Tests.SnapshotTests.KitchenSinkSnapshotTest (kitchenSinkSnapshotTest) where

import qualified Data.Text as T
import E2E
import Tests.SnapshotTests.SdkPackageExports (assertSdkPackageExportsExist)
import Wasp.Project.Db (databaseUrlEnvVarName)

kitchenSinkSnapshotTest :: E2ETest
kitchenSinkSnapshotTest =
  snapshotTest "kitchen-sink" $ do
    copyGitTrackedDir "examples/kitchen-sink" waspAppDirName
    inDirectory waspAppDirName $ do
      copyFile ".env.server.example" ".env.server"
      appendToFile ".env.server" ("\n" <> T.pack databaseUrlEnvVarName <> "=mock-database-url")
      wasp ["install"]
      wasp ["compile"]
      assertSdkPackageExportsExist
