module Tests.SnapshotTests.WaspNewSnapshotTest (waspNewSnapshotTest) where

import SnapshotTest (SnapshotTest, makeSnapshotTest)
import Steps (createWaspProject)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspNewSnapshotTest :: SnapshotTest
waspNewSnapshotTest =
  makeSnapshotTest "wasp-new" (createWaspProject minimalStarterTemplate)
