module EphemeralTest.WaspInfoEphemeralTest (waspInfoEphemeralTest) where

import EphemeralTest (EphemeralTest, makeEphemeralTest, makeEphemeralTestCase)
import EphemeralTest.ShellCommands (createEphemeralWaspProject, withInEphemeralWaspProjectDir)
import WaspProject.ShellCommands (waspCliInfo)
import ShellCommands (WaspNewTemplate(..))

-- TODO: Test `wasp info` values change properly: 
-- name, database, project dir size, last compile.
waspInfoEphemeralTest :: EphemeralTest
waspInfoEphemeralTest =
  makeEphemeralTest
    "wasp-info"
    [ makeEphemeralTestCase
        "Should fail outside of a Wasp project"
        (return "! wasp-cli info"), -- we must use string here bacuse it's not a WaspProjectContext
      makeEphemeralTestCase
        "Setup: Create Wasp project from minimal starter"
        (createEphemeralWaspProject Minimal),
      makeEphemeralTestCase
        "Should succeed inside of a Wasp project"
        (withInEphemeralWaspProjectDir [waspCliInfo])
    ]
