module EphemeralTest.WaspDepsEphemeralTest (waspDepsEphemeralTest) where

import EphemeralTest (EphemeralTest, makeEphemeralTest, makeEphemeralTestCase)
import EphemeralTest.ShellCommands (createEphemeralWaspProject, withInEphemeralWaspProjectDir)
import ShellCommands (WaspNewTemplate (..))

-- TODO: Test `wasp deps` deps chage with installs/uninstalls.
waspDepsEphemeralTest :: EphemeralTest
waspDepsEphemeralTest =
  makeEphemeralTest
    "wasp-deps"
    [ makeEphemeralTestCase
        "Should fail outside of a Wasp project"
        (return "! wasp-cli deps"),
      makeEphemeralTestCase
        "Setup: Create Wasp project from minimal starter"
        (createEphemeralWaspProject Minimal),
      makeEphemeralTestCase
        "Should succeed inside of a Wasp project"
        (withInEphemeralWaspProjectDir [return "wasp-cli deps"])
    ]
