module Tests.WaspInspectTest (waspInspectTest) where

import ShellCommands (ShellCommand, createTestWaspProject, inTestWaspProjectDir, waspCliInspect, waspCliInspectJson)
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspInspectTest :: Test
waspInspectTest =
  Test
    "wasp-inspect"
    [ TestCase
        "fail-outside-project"
        (return [waspCliInspectFails]),
      TestCase
        "succeed-inside-project"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ waspCliInspect,
                  waspCliInspectJson
                ]
            ]
        )
    ]
  where
    waspCliInspectFails :: ShellCommand
    waspCliInspectFails = "! $WASP_CLI_CMD inspect"
