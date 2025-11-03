module EphemeralTest.WaspDbSeedEphemeralTest (waspDbSeedEphemeralTest) where

import EphemeralTest (EphemeralTest, makeEphemeralTest, makeEphemeralTestCase)
import EphemeralTest.ShellCommands (createEphemeralWaspProjectFromMinimalStarter)

waspDbSeedEphemeralTest :: EphemeralTest
waspDbSeedEphemeralTest =
  makeEphemeralTest
    "wasp-db-seed"
    [ makeEphemeralTestCase
        "Should fail outside of a Wasp project"
        (return "! wasp-cli db seed"),
      makeEphemeralTestCase
        "Setup: Create Wasp project from minimal starter"
        createEphemeralWaspProjectFromMinimalStarter

      -- NOTE: kitchen-sink has pre-built seed scripts, but do we want something more simple?
      -- e.g. add 1 task
        
      -- should ensure the database is empty (but migrated)
      -- should seed succesfully
      -- should ensure the databse is no longer empty
    ]

