module Tests.SnapshotTests.WaspCompileSnapshotTest (waspCompileSnapshotTest) where

import E2e
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspCompileSnapshotTest :: E2eTest
waspCompileSnapshotTest =
  snapshotTest "wasp-compile"
    $ withNewWaspProject minimalStarterTemplate
    $ wasp ["compile"]
