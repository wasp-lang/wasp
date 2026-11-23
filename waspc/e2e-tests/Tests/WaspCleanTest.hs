module Tests.WaspCleanTest (waspCleanTest) where

import E2E
import qualified StrongPath as SP
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Project.Common (dotWaspDirInWaspProjectDir)

waspCleanTest :: E2ETest
waspCleanTest =
  testGroup
    "wasp-clean"
    [ test "fail-outside-project" $ waspFails ["clean"],
      test "succeed-uncompiled-project" $
        withNewWaspProject minimalStarterTemplate $ do
          wasp ["clean"]
          assertDirectoryDoesNotExist dotWaspDir
          assertDirectoryDoesNotExist "node_modules",
      test "succeed-compiled-project" $
        withNewWaspProject minimalStarterTemplate $ do
          wasp ["compile"]
          wasp ["clean"]
          assertDirectoryDoesNotExist dotWaspDir
          assertDirectoryDoesNotExist "node_modules"
    ]
  where
    -- Not a literal, since a check that a dir doesn't exist would still pass
    -- if Wasp renamed it.
    dotWaspDir = SP.fromRelDir dotWaspDirInWaspProjectDir
