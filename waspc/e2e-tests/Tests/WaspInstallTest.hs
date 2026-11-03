module Tests.WaspInstallTest (waspInstallTest) where

import E2e
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspInstallTest :: E2eTest
waspInstallTest =
  testGroup
    "wasp-install"
    [ test "install-fails-outside-project" $ waspFails ["install"],
      test "install-restores-wasp-spec-after-clean" $
        withNewWaspProject minimalStarterTemplate $ do
          wasp ["clean"]
          assertDirectoryDoesNotExist "node_modules"
          wasp ["install"]
          assertSymlinkExists "node_modules/@wasp/spec"
          wasp ["compile"]
    ]
