module Tests.WaspDockerfileTest (waspDockerfileTests) where

import E2e
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- TODO: Test `wasp dockerfile` content.
waspDockerfileTests :: [E2eTest]
waspDockerfileTests =
  testGroup
    "wasp-dockerfile"
    [ test "fail-outside-project" $ waspFails ["dockerfile"],
      test "succeed-inside-project" $ withNewWaspProject minimalStarterTemplate $ wasp ["dockerfile"]
    ]
