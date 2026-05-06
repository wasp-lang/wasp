module Tests.WaspInfoTest (waspInfoTest) where

import ShellCommands (ShellCommand, createTestWaspProject, inTestWaspProjectDir, waspCliInfo)
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
        (return [waspCliInfoFails]),
      TestCase
        "succeed-inside-project"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ waspCliInfo
                ]
            ]
        )
    ]
  where
    waspCliInfoFails :: ShellCommand
    waspCliInfoFails = "! wasp-cli info"
