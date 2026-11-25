module Tests.SnapshotTests.WaspMigrateSnapshotTest (test_waspMigrateSnapshot) where

import E2E
import Tests.Fixtures (taskPrismaModel)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

test_waspMigrateSnapshot :: TestTree
test_waspMigrateSnapshot =
  snapshotTest "wasp-migrate" $
    withNewWaspProject minimalStarterTemplate $ do
      wasp ["compile"]
      appendToFile "schema.prisma" taskPrismaModel
      waspDbMigrateDev "foo"
