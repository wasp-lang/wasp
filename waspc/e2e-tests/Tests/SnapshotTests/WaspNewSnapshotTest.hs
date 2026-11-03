module Tests.SnapshotTests.WaspNewSnapshotTest (waspNewSnapshotTest) where

import E2e
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspNewSnapshotTest :: E2eTest
waspNewSnapshotTest = snapshotTest "wasp-new" $ createWaspProject minimalStarterTemplate
