module EphemeralTest.WaspBuildStartEphemeralTest (waspBuildStartEphemeralTest) where

import EphemeralTest (EphemeralTest, makeEphemeralTest, makeEphemeralTestCase)
import EphemeralTest.ShellCommands (createEphemeralWaspProject, withInEphemeralWaspProjectDir)
import WaspProject.ShellCommands (setWaspDbToPSQL, waspCliBuild, waspCliBuildStart)
import ShellCommands (WaspNewTemplate(..), ShellCommandBuilder, ShellCommand)

-- FIXME: @waspCliBuildStart@ - figure out long lasting processes
waspBuildStartEphemeralTest :: EphemeralTest
waspBuildStartEphemeralTest =
  makeEphemeralTest
    "wasp-build-start"
    [ makeEphemeralTestCase
        "Should fail outside of a Wasp project"
        waspCliBuildStartFails,
      makeEphemeralTestCase
        "Setup: Create Wasp project from minimal starter"
        (createEphemeralWaspProject Minimal),
      makeEphemeralTestCase
        "Setup: Modify Wasp project to use Postgresql"
        (withInEphemeralWaspProjectDir [setWaspDbToPSQL]),
      makeEphemeralTestCase
        "Should fail inside of a unbuilt Wasp project"
        (withInEphemeralWaspProjectDir [waspCliBuildStart "-s DATABASE_URL=none"]),
      makeEphemeralTestCase
        "Setup: Build the Wasp project"
        (withInEphemeralWaspProjectDir [waspCliBuild]),
      makeEphemeralTestCase
        "Should succeed inside of a built Wasp project"
        (withInEphemeralWaspProjectDir [waspCliBuildStart "-s DATABASE_URL=none"])
    ]
    where
      waspCliBuildStartFails :: ShellCommandBuilder context ShellCommand
      waspCliBuildStartFails = return "! wasp-cli build start"
