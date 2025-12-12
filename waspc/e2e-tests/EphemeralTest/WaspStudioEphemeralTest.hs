module EphemeralTest.WaspStudioEphemeralTest (waspStudioEphemeralTest) where

import EphemeralTest (EphemeralTest, makeEphemeralTest, makeEphemeralTestCase)
import EphemeralTest.ShellCommands (createEphemeralWaspProject, withInEphemeralWaspProjectDir)
import ShellCommands (ShellCommand, ShellCommandBuilder, WaspNewTemplate (..))
import WaspProject.ShellCommands (waspCliStudio)

-- | NOTE: Once it evolves it will probably have it’s own
-- playwright tests inside of the TS package.
-- So we will tests just that the command works.
-- FIXME: @waspCliStudio@ - figure out long lasting processes
waspStudioEphemeralTest :: EphemeralTest
waspStudioEphemeralTest =
  makeEphemeralTest
    "wasp-studio"
    [ makeEphemeralTestCase
        "Should fail outside of a Wasp project"
        waspCliStudioFails,
      makeEphemeralTestCase
        "Setup: Create Wasp project from minimal starter"
        (createEphemeralWaspProject Minimal),
      makeEphemeralTestCase
        "Should succeed inside of a uncompiled Wasp project"
        (withInEphemeralWaspProjectDir [waspCliStudio])
    ]
  where
    waspCliStudioFails :: ShellCommandBuilder context ShellCommand
    waspCliStudioFails = return "! wasp-cli studio"
