module EphemeralTest.WaspDepsEphemeralTest (waspDepsEphemeralTest) where

import EphemeralTest (EphemeralTest, makeEphemeralTest, makeEphemeralTestCase)
import EphemeralTest.ShellCommands (createEphemeralWaspProjectFromMinimalStarter, withInEphemeralWaspProjectDir)

-- TODO: test deps changing with installs/uninstalls
waspDepsEphemeralTest :: EphemeralTest
waspDepsEphemeralTest =
  makeEphemeralTest
    "wasp-deps"
    [ makeEphemeralTestCase
        "Should fail outside of a Wasp project"
        (return "! wasp-cli deps"),
      makeEphemeralTestCase
        "Setup: Create Wasp project from minimal starter"
        createEphemeralWaspProjectFromMinimalStarter,
      makeEphemeralTestCase
        "Should succeed inside of a Wasp project"
        (withInEphemeralWaspProjectDir [return "wasp-cli deps"])
    ]
