module Tests.WaspCleanTest (waspCleanTest) where

import E2E
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspCleanTest :: E2ETest
waspCleanTest =
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
