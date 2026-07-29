module Tests.SnapshotTests.WaspNewSnapshotTest (waspNewSnapshotTest) where

import SharedActions (createWaspProject)
import SnapshotTest (SnapshotTest, makeSnapshotTest)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspNewSnapshotTest :: SnapshotTest
waspNewSnapshotTest =
  makeSnapshotTest "wasp-new" (createWaspProject minimalStarterTemplate)
