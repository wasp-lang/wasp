module Tests.WaspDbStartTest (waspDbStartTest) where

import Steps (createTestWaspProject, inTestWaspProjectDir, runCommand, runCommandExpectingFailure, setWaspDbToPSQL, waspCliDbStart)
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- FIXME: @waspCliDbStart@ - figure out long lasting processes
waspDbStartTest :: Test
waspDbStartTest =
  Test
    "wasp-db-start"
    [ TestCase
        "fail-outside-project"
        [runCommandExpectingFailure waspCliDbStart],
      TestCase
        "succeed-sqlite-project"
        [ createTestWaspProject minimalStarterTemplate,
          inTestWaspProjectDir
            [ runCommand waspCliDbStart
            ]
        ],
      TestCase
        "succeed-postgresql-project"
        [ createTestWaspProject minimalStarterTemplate,
          inTestWaspProjectDir
            [ setWaspDbToPSQL,
              runCommand waspCliDbStart
            ]
        ]
    ]
