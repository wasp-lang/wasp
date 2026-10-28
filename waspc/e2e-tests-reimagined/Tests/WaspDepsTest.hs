module Tests.WaspDepsTest (waspDepsTests) where

import E2e
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- TODO: Test that deps change with installs/uninstalls.
waspDepsTests :: [E2eTest]
waspDepsTests =
  testGroup
    "wasp-deps"
    [ test "fail-outside-project" $ waspFails ["deps"],
      test "succeed-inside-project" $ withNewWaspProject minimalStarterTemplate $ wasp ["deps"]
    ]
