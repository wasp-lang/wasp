module Tests.WaspCleanTest (waspCleanTests) where

import E2e
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspCleanTests :: [E2eTest]
waspCleanTests =
  testGroup
    "wasp-clean"
    [ test "fail-outside-project" $ waspFails ["clean"],
      test "succeed-uncompiled-project" $
        withNewWaspProject minimalStarterTemplate $ do
          wasp ["clean"]
          assertDirectoryDoesNotExist ".wasp"
          assertDirectoryDoesNotExist "node_modules",
      test "succeed-compiled-project" $
        withNewWaspProject minimalStarterTemplate $ do
          wasp ["compile"]
          wasp ["clean"]
          assertDirectoryDoesNotExist ".wasp"
          assertDirectoryDoesNotExist "node_modules"
    ]
