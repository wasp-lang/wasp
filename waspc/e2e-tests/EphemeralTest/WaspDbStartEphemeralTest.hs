module EphemeralTest.WaspDbStartEphemeralTest (waspDbStartEphemeralTest) where

import EphemeralTest (EphemeralTest, makeEphemeralTest, makeEphemeralTestCase)
import EphemeralTest.ShellCommands (createEphemeralWaspProject, withInEphemeralWaspProjectDir)
import ShellCommands (WaspNewTemplate (..), ShellCommandBuilder, ShellCommand)
import WaspProject.ShellCommands (setWaspDbToPSQL, waspCliDbStart)

-- FIXME: @waspCliDbStart@ - figure out long lasting processes
waspDbStartEphemeralTest :: EphemeralTest
waspDbStartEphemeralTest =
  makeEphemeralTest
    "wasp-db-start"
    [ makeEphemeralTestCase
        "Should fail outside of a Wasp project"
        waspCliDbStartFails,
      makeEphemeralTestCase
        "Setup: Create Wasp project from minimal starter"
        (createEphemeralWaspProject Minimal),
      makeEphemeralTestCase
        "Should exit early and successfully inside of a SQLite Wasp project"
        (withInEphemeralWaspProjectDir [waspCliDbStart]),
      makeEphemeralTestCase
        "Setup: Modify Wasp project to use Postgresql"
        (withInEphemeralWaspProjectDir [setWaspDbToPSQL]),
      makeEphemeralTestCase
        "Should succeed inside of a Wasp project"
        (withInEphemeralWaspProjectDir [waspCliDbStart])
    ]
  where
    waspCliDbStartFails :: ShellCommandBuilder context ShellCommand
    waspCliDbStartFails = return "! wasp-cli db start"
