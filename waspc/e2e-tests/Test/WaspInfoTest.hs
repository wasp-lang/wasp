module Test.WaspInfoTest (waspInfoTest) where

import ShellCommands (ShellCommand, WaspNewTemplate (..))
import Test (Test, makeTest, makeTestCase)
import Test.ShellCommands (createTestWaspProject, withInTestWaspProjectDir)
import WaspProject.ShellCommands (waspCliInfo)

-- TODO: Test `wasp info` values change properly:
-- name, database, project dir size, last compile.
waspInfoTest :: Test
waspInfoTest =
  makeTest
    "wasp-info"
    [ makeTestCase
        "Should fail outside of a Wasp project"
        (return [waspCliInfoFails]),
      makeTestCase
        "Should succeed inside of a Wasp project"
        ( sequence
            [ createTestWaspProject Minimal,
              withInTestWaspProjectDir [waspCliInfo]
            ]
        )
    ]
  where
    waspCliInfoFails :: ShellCommand
    waspCliInfoFails = "! wasp-cli info"
