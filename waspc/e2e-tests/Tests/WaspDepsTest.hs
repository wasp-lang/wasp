module Tests.WaspDepsTest (waspDepsTest) where

import Steps (createTestWaspProject, inTestWaspProjectDir, runCommand, runCommandExpectingFailure, waspCliDeps)
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- TODO: Test that deps change with installs/uninstalls.
waspDepsTest :: Test
waspDepsTest =
  Test
    "wasp-deps"
    [ TestCase
        "fail-outside-project"
        (sequence [runCommandExpectingFailure waspCliDeps]),
      TestCase
        "succeed-inside-project"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ runCommand waspCliDeps
                ]
            ]
        )
    ]
