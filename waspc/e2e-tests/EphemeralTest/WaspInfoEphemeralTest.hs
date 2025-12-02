module EphemeralTest.WaspInfoEphemeralTest (waspInfoEphemeralTest) where

import EphemeralTest (EphemeralTest, makeEphemeralTest, makeEphemeralTestCase)
import EphemeralTest.ShellCommands (createEphemeralWaspProject, withInEphemeralWaspProjectDir)
import ShellCommands (ShellCommand, ShellCommandBuilder, WaspNewTemplate (..))
import WaspProject.ShellCommands (waspCliInfo)

-- TODO: Test `wasp info` values change properly:
-- name, database, project dir size, last compile.
waspInfoEphemeralTest :: EphemeralTest
waspInfoEphemeralTest =
  makeEphemeralTest
    "wasp-info"
    [ makeEphemeralTestCase
        "Should fail outside of a Wasp project"
        waspCliInfoFails,
      makeEphemeralTestCase
        "Setup: Create Wasp project from minimal starter"
        (createEphemeralWaspProject Minimal),
      makeEphemeralTestCase
        "Should succeed inside of a Wasp project"
        (withInEphemeralWaspProjectDir [waspCliInfo])
    ]
  where
    waspCliInfoFails :: ShellCommandBuilder context ShellCommand
    waspCliInfoFails = return "! wasp-cli info"
