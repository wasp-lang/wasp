module Tests.WaspShowTest (waspShowTest) where

import ShellCommands (ShellCommand, createTestWaspProject, inTestWaspProjectDir, waspCliShowBuild, waspCliShowBuildJson, waspCliShowSpec, waspCliShowSpecJson)
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
        (return [waspCliShowSpecFails, waspCliShowBuildFails]),
      TestCase
        "succeed-inside-project"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ waspCliShowSpec,
                  waspCliShowSpecJson,
                  waspCliShowBuild,
                  waspCliShowBuildJson
                ]
            ]
        )
    ]
  where
    waspCliShowFails :: ShellCommand
    waspCliShowFails = "! $WASP_CLI_CMD show"

    waspCliShowSpecFails :: ShellCommand
    waspCliShowSpecFails = "! $WASP_CLI_CMD show spec"

    waspCliShowBuildFails :: ShellCommand
    waspCliShowBuildFails = "! $WASP_CLI_CMD show build"
