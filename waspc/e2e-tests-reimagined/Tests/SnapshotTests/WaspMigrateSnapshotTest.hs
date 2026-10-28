module Tests.SnapshotTests.WaspMigrateSnapshotTest (waspMigrateSnapshotTest) where

import E2e
import Tests.Fixtures (taskPrismaModel)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspMigrateSnapshotTest :: E2eTest
waspMigrateSnapshotTest =
  snapshotTest "wasp-migrate" $
    withNewWaspProject minimalStarterTemplate $ do
      wasp ["compile"]
      appendToPrismaSchema taskPrismaModel
      waspDbMigrateDev "foo"
