module EphemeralTest.WaspDbStartEphemeralTest (waspDbStartEphemeralTest) where

import EphemeralTest (EphemeralTest, makeEphemeralTest, makeEphemeralTestCase)
import EphemeralTest.ShellCommands (createEphemeralWaspProject, withInEphemeralWaspProjectDir)
import WaspProject.ShellCommands (waspCliDbStart, setWaspDbToPSQL)
import ShellCommands (WaspNewTemplate(..))

waspDbStartEphemeralTest :: EphemeralTest
waspDbStartEphemeralTest =
  makeEphemeralTest
    "wasp-db-start"
    [ makeEphemeralTestCase
        "Should fail outside of a Wasp project"
        (return "! wasp-cli db start"),
      makeEphemeralTestCase
        "Setup: Create Wasp project from minimal starter"
        (createEphemeralWaspProject Minimal),
      makeEphemeralTestCase
        "Should exit early and successfully inside of a SQLite Wasp project"
        (withInEphemeralWaspProjectDir [waspCliDbStart]),
      makeEphemeralTestCase
        "Setup: Modify Wasp project to use Postgresql"
        (withInEphemeralWaspProjectDir [setWaspDbToPSQL])
      -- questions: 
      -- how do we run db non-interactively?
      -- how do we know when the db is started?
      -- how do we stop the process?
    ]

