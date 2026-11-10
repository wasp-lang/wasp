module Tests.WaspDepsTest (waspDepsTest) where

import E2E
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- TODO: Test that deps change with installs/uninstalls.
waspDepsTest :: E2ETest
waspDepsTest =
  testGroup
    "wasp-deps"
    [ test "fail-outside-project" $ waspFails ["deps"],
      test "succeed-inside-project"
        $ withNewWaspProject minimalStarterTemplate
        $ wasp ["deps"]
    ]
