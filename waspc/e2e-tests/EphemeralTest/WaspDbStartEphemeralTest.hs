module EphemeralTest.WaspDbStartEphemeralTest (waspDbStartEphemeralTest) where

import EphemeralTest (EphemeralTest, makeEphemeralTest, makeEphemeralTestCase)
import EphemeralTest.ShellCommands (createEphemeralWaspProjectFromMinimalStarter, withInEphemeralWaspProjectDir)
import WaspProject.ShellCommands (waspCliDbStart)

waspDbStartEphemeralTest :: EphemeralTest
waspDbStartEphemeralTest =
  makeEphemeralTest
    "wasp-db-start"
    [ makeEphemeralTestCase
        "Should fail outside of a Wasp project"
        (return "! wasp-cli db start"),
      makeEphemeralTestCase
        "Setup: Create Wasp project from minimal starter"
        createEphemeralWaspProjectFromMinimalStarter,
      makeEphemeralTestCase
        "Should exit early and successfully inside of a SQLite Wasp project"
        (withInEphemeralWaspProjectDir [waspCliDbStart])
      -- questions: 
      -- how do we run db non-interactively?
      -- how do we know when the db is started?
      -- how do we stop the process?
    ]

