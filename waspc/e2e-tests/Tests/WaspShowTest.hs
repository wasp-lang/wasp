module Tests.WaspShowTest (waspShowTest) where

import ShellCommands (ShellCommand, createTestWaspProject, inTestWaspProjectDir, waspCliShowBuild, waspCliShowBuildJson)
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspShowTest :: Test
waspShowTest =
  Test
    "wasp-show"
    [ TestCase
        "fail-without-subcommand"
        (return [waspCliShowFails]),
      TestCase
        "fail-outside-project"
        (return [waspCliShowBuildFails]),
      TestCase
        "succeed-inside-project"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ waspCliShowBuild,
                  waspCliShowBuildJson
                ]
            ]
        )
    ]
  where
    waspCliShowFails :: ShellCommand
    waspCliShowFails = "! $WASP_CLI_CMD show"

    waspCliShowBuildFails :: ShellCommand
    waspCliShowBuildFails = "! $WASP_CLI_CMD show build"
