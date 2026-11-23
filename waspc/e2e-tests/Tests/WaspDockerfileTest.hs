module Tests.WaspDockerfileTest (waspDockerfileTest) where

import E2E
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- TODO: Test `wasp dockerfile` content.
waspDockerfileTest :: E2ETest
waspDockerfileTest =
  testGroup
    "wasp-dockerfile"
    [ test "fail-outside-project" $ waspFailsOutsideProject ["dockerfile"],
      test "succeed-inside-project"
        $ withNewWaspProject minimalStarterTemplate
        $ wasp ["dockerfile"]
          >>= assertStdoutContains "FROM node"
    ]
