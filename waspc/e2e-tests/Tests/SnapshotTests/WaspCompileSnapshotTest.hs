module Tests.SnapshotTests.WaspCompileSnapshotTest (waspCompileSnapshotTest) where

import E2E
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspCompileSnapshotTest :: E2ETest
waspCompileSnapshotTest =
  snapshotTest "wasp-compile"
    $ withNewWaspProject minimalStarterTemplate
    $ wasp ["compile"]
