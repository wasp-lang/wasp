module Tests.WaspDepsTest (test_waspDeps) where

import E2E
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- TODO: Test that deps change with installs/uninstalls.
test_waspDeps :: TestTree
test_waspDeps =
  testGroup
    "wasp-deps"
    [ test "fail-outside-project" $ waspFailsOutsideProject ["deps"],
      test "succeed-inside-project"
        $ withNewWaspProject minimalStarterTemplate
        $ wasp ["deps"]
          >>= assertStdoutContains "Server dependencies:"
    ]
