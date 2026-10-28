module Tests.WaspInstallTest (waspInstallTests) where

import E2e
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspInstallTests :: [E2eTest]
waspInstallTests =
  testGroup
    "wasp-install"
    [ test "install-fails-outside-project" $ waspFails ["install"],
      test "install-restores-wasp-spec-after-clean" $
        withNewWaspProject minimalStarterTemplate $ do
          wasp ["clean"]
          assertDirectoryDoesNotExist "node_modules"
          wasp ["install"]
          assertSymlinkExists "node_modules/@wasp.sh/spec"
          wasp ["compile"]
    ]
