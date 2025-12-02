module EphemeralTest.WaspDbStudioEphemeralTest (waspDbStudioEphemeralTest) where

import EphemeralTest (EphemeralTest, makeEphemeralTest, makeEphemeralTestCase)
import EphemeralTest.ShellCommands (createEphemeralWaspProject, withInEphemeralWaspProjectDir)
import ShellCommands (ShellCommand, ShellCommandBuilder, WaspNewTemplate (..))
import WaspProject.ShellCommands (waspCliDbStudio)

-- | NOTE: We don't test feature content since it's prisma feature.
-- FIXME: @waspCliDbStudio@ - figure out long lasting processes
waspDbStudioEphemeralTest :: EphemeralTest
waspDbStudioEphemeralTest =
  makeEphemeralTest
    "wasp-db-studio"
    [ makeEphemeralTestCase
        "Should fail outside of a Wasp project"
        waspCliDbStudioFails,
      makeEphemeralTestCase
        "Setup: Create Wasp project from minimal starter"
        (createEphemeralWaspProject Minimal),
      makeEphemeralTestCase
        "Should succeed inside of a uncompiled Wasp project"
        (withInEphemeralWaspProjectDir [waspCliDbStudio])
    ]
  where
    waspCliDbStudioFails :: ShellCommandBuilder context ShellCommand
    waspCliDbStudioFails = return "! wasp-cli db studio"
