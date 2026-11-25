module Tests.SnapshotTests.WaspCompileSnapshotTest (test_waspCompileSnapshot) where

import E2E
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

test_waspCompileSnapshot :: TestTree
test_waspCompileSnapshot =
  snapshotTest "wasp-compile"
    $ withNewWaspProject minimalStarterTemplate
    $ wasp ["compile"]
