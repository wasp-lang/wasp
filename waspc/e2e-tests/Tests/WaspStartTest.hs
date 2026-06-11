module Tests.WaspStartTest (waspStartTest) where

import Steps (assertDirExists, createTestWaspProject, inTestWaspProjectDir, runCommand, runCommandExpectingFailure, waspCliCompile, waspCliStart)
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- FIXME: @waspCliStart@ - figure out long lasting processes
waspStartTest :: Test
waspStartTest =
  Test
    "wasp-start"
    [ TestCase
        "fail-outside-project"
        (sequence [runCommandExpectingFailure waspCliStart]),
      TestCase
        "succeed-uncompiled-project"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ runCommand waspCliStart,
                  assertDirExists ".wasp",
                  assertDirExists "node_modules"
                ]
            ]
        ),
      TestCase
        "succeed-compiled-project"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ runCommand waspCliCompile,
                  runCommand waspCliStart
                ]
            ]
        )
    ]
