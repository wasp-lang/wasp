module Tests.WaspInfoTest (waspInfoTest) where

import ShellCommands (ShellCommand, WaspNewTemplate (..))
import Test (Test, makeTest, makeTestCase)
import Test.ShellCommands (createTestWaspProject, inTestWaspProjectDir)
import WaspProject.ShellCommands (waspCliInfo)

-- TODO: Test `wasp info` values change properly:
-- name, database, project dir size, last compile.
waspInfoTest :: Test
waspInfoTest =
  makeTest
    "wasp-info"
    [ makeTestCase
        "fail-outside-project"
        (return [waspCliInfoFails]),
      makeTestCase
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
