module Tests.WaspInfoTest (waspInfoTest) where

import ShellCommands (ShellCommand, WaspNewTemplate (..), createTestWaspProject, inTestWaspProjectDir, waspCliInfo)
import Test (Test (..), TestCase (..))

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
