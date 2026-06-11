module Tests.WaspCleanTest (waspCleanTest) where

import Steps
  ( assertDirDoesNotExist,
    createTestWaspProject,
    inTestWaspProjectDir,
    runCommand,
    runCommandExpectingFailure,
    waspCliClean,
    waspCliCompile,
  )
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspCleanTest :: Test
waspCleanTest =
  Test
    "wasp-clean"
    [ TestCase
        "fail-outside-project"
        (sequence [runCommandExpectingFailure waspCliClean]),
      TestCase
        "succeed-uncompiled-project"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ runCommand waspCliClean,
                  assertDirDoesNotExist ".wasp",
                  assertDirDoesNotExist "node_modules"
                ]
            ]
        ),
      TestCase
        "succeed-compiled-project"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ runCommand waspCliCompile,
                  runCommand waspCliClean,
                  assertDirDoesNotExist ".wasp",
                  assertDirDoesNotExist "node_modules"
                ]
            ]
        )
    ]
