module Tests.WaspInstallTest (test_waspInstall) where

import E2E
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

test_waspInstall :: TestTree
test_waspInstall =
  testGroup
    "wasp-install"
    [ test "fail-outside-project" $ waspFailsOutsideProject ["install"],
      test "succeed-restore-wasp-spec-after-clean" $
        withNewWaspProject minimalStarterTemplate $ do
          wasp ["clean"]
          assertDirectoryDoesNotExist "node_modules"
          wasp ["install"]
          assertSymlinkExists "node_modules/@wasp/spec"
          wasp ["compile"]
    ]
