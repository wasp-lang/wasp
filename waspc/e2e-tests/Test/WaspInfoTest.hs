module Test.WaspInfoTest (waspInfoTest) where

import ShellCommands (ShellCommand, ShellCommandBuilder, WaspNewTemplate (..))
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
        waspCliInfoFails,
      makeTestCase
        "Setup: Create Wasp project from minimal starter"
        (createTestWaspProject Minimal),
      makeTestCase
        "Should succeed inside of a Wasp project"
        (withInTestWaspProjectDir [waspCliInfo])
    ]
  where
    waspCliInfoFails :: ShellCommandBuilder context ShellCommand
    waspCliInfoFails = return "! wasp-cli info"
