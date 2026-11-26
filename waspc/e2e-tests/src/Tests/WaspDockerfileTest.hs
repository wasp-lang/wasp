module Tests.WaspDockerfileTest (test_waspDockerfile) where

import E2E
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- TODO: Test `wasp dockerfile` content.
test_waspDockerfile :: TestTree
test_waspDockerfile =
  testGroup
    "wasp-dockerfile"
    [ test "fail-outside-project" $ waspFailsOutsideProject ["dockerfile"],
      test "succeed-inside-project"
        $ withNewWaspProject minimalStarterTemplate
        $ wasp ["dockerfile"]
          >>= assertStdoutContains "FROM node"
    ]
