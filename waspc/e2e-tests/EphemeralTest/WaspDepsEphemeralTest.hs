module EphemeralTest.WaspDepsEphemeralTest (waspDepsEphemeralTest) where

import EphemeralTest (EphemeralTest, makeEphemeralTest, makeEphemeralTestCase)
import EphemeralTest.ShellCommands (createEphemeralWaspProject, withInEphemeralWaspProjectDir)
import ShellCommands (WaspNewTemplate (..), ShellCommandBuilder, ShellCommand)
import WaspProject.ShellCommands (waspCliDeps)

-- TODO: Test that deps change with installs/uninstalls.
waspDepsEphemeralTest :: EphemeralTest
waspDepsEphemeralTest =
  makeEphemeralTest
    "wasp-deps"
    [ makeEphemeralTestCase
        "Should fail outside of a Wasp project"
        waspCliDepsFails,
      makeEphemeralTestCase
        "Setup: Create Wasp project from minimal starter"
        (createEphemeralWaspProject Minimal),
      makeEphemeralTestCase
        "Should succeed inside of a Wasp project"
        (withInEphemeralWaspProjectDir [waspCliDeps])
    ]
  where
    waspCliDepsFails :: ShellCommandBuilder context ShellCommand
    waspCliDepsFails = return "! wasp-cli deps"
