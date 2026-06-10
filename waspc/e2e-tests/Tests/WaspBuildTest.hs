module Tests.WaspBuildTest (waspBuildTest) where

import Steps
  ( assertDirExists,
    createTestWaspProject,
    inTestWaspProjectDir,
    runCommand,
    runCommandExpectingFailure,
    setWaspDbToPSQL,
    waspCliBuild,
  )
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspBuildTest :: Test
waspBuildTest =
  Test
    "wasp-build"
    [ TestCase
        "fail-outside-project"
        [runCommandExpectingFailure waspCliBuild],
      TestCase
        "fail-sqlite-project"
        [ createTestWaspProject minimalStarterTemplate,
          inTestWaspProjectDir [runCommandExpectingFailure waspCliBuild]
        ],
      TestCase
        "succeed-postgresql-project"
        [ createTestWaspProject minimalStarterTemplate,
          inTestWaspProjectDir
            [ setWaspDbToPSQL,
              runCommand waspCliBuild,
              assertDirExists ".wasp",
              assertDirExists "node_modules"
            ]
        ]
    ]
