module Tests.WaspInfoTest (waspInfoTest) where

import ShellCommands (ShellCommand, WaspNewTemplate (..))
import Test (Test (..), TestCase (..))
import Test.ShellCommands (createTestWaspProject, inTestWaspProjectDir)
import WaspProject.ShellCommands (waspCliInfo)

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
            [ createTestWaspProject Minimal,
              inTestWaspProjectDir
                [ waspCliInfo
                ]
            ]
        )
    ]
  where
    waspCliInfoFails :: ShellCommand
    waspCliInfoFails = "! wasp-cli info"
