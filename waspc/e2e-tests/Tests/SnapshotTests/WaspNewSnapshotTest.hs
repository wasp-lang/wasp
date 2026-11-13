module Tests.SnapshotTests.WaspNewSnapshotTest (waspNewSnapshotTest) where

import E2E
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspNewSnapshotTest :: E2ETest
waspNewSnapshotTest = snapshotTest "wasp-new" $ createWaspProject minimalStarterTemplate
