module Tests.WaspBuildStartTest (waspBuildStartTest) where

import ShellCommands (ShellCommand, createTestWaspProject, inTestWaspProjectDir, setWaspDbToPSQL, waspCliBuild, waspCliBuildStart)
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

-- FIXME: @waspCliBuildStart@ - figure out long lasting processes
waspBuildStartTest :: Test
waspBuildStartTest =
  Test
    "wasp-build-start"
    [ TestCase
        "fail-outside-project"
        (return [waspCliBuildStartFails]),
      TestCase
        "fail-unbuilt-project"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ setWaspDbToPSQL,
                  return waspCliBuildStartFails
                ]
            ]
        ),
      TestCase
        "succeed-built-project"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ setWaspDbToPSQL,
                  waspCliBuild,
                  waspCliBuildStart "-s DATABASE_URL=none"
                ]
            ]
        )
    ]
  where
    waspCliBuildStartFails :: ShellCommand
    waspCliBuildStartFails = "! wasp-cli build start"
