module Tests.SnapshotTests.WaspNewSnapshotTest (test_waspNewSnapshot) where

import E2E
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

test_waspNewSnapshot :: TestTree
test_waspNewSnapshot = snapshotTest "wasp-new" $ createWaspProject minimalStarterTemplate
