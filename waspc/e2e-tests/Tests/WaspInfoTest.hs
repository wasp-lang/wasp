module Tests.WaspInfoTest (waspInfoTest) where

import Steps (createTestWaspProject, inTestWaspProjectDir, runCommand, runCommandExpectingFailure, waspCliInfo)
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- TODO: Test `wasp info` values change properly:
-- name, database, project dir size, last compile.
waspInfoTest :: Test
waspInfoTest =
  Test
    "wasp-info"
    [ TestCase
        "fail-outside-project"
        [runCommandExpectingFailure waspCliInfo],
      TestCase
        "succeed-inside-project"
        [ createTestWaspProject minimalStarterTemplate,
          inTestWaspProjectDir
            [ runCommand waspCliInfo
            ]
        ]
    ]
