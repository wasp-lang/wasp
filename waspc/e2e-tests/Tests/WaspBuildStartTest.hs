module Tests.WaspBuildStartTest (waspBuildStartTest) where

import Steps (createTestWaspProject, inTestWaspProjectDir, runCommand, runCommandExpectingFailure, setWaspDbToPSQL, waspCliBuild, waspCliBuildStart)
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- FIXME: @waspCliBuildStart@ - figure out long lasting processes
waspBuildStartTest :: Test
waspBuildStartTest =
  Test
    "wasp-build-start"
    [ TestCase
        "fail-outside-project"
        [runCommandExpectingFailure $ waspCliBuildStart []],
      TestCase
        "fail-unbuilt-project"
        [ createTestWaspProject minimalStarterTemplate,
          inTestWaspProjectDir
            [ setWaspDbToPSQL,
              runCommandExpectingFailure $ waspCliBuildStart []
            ]
        ],
      TestCase
        "succeed-built-project"
        [ createTestWaspProject minimalStarterTemplate,
          inTestWaspProjectDir
            [ setWaspDbToPSQL,
              runCommand waspCliBuild,
              runCommand $ waspCliBuildStart ["-s", "DATABASE_URL=none"]
            ]
        ]
    ]
