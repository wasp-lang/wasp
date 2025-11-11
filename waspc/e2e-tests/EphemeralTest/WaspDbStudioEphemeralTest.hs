module EphemeralTest.WaspDbStudioEphemeralTest (waspDbStudioEphemeralTest) where

import EphemeralTest (EphemeralTest, makeEphemeralTest, makeEphemeralTestCase)
import EphemeralTest.ShellCommands (createEphemeralWaspProject)
import ShellCommands (WaspNewTemplate(..))

-- | NOTE: We don't test feature content since it's prisma feature. 
waspDbStudioEphemeralTest :: EphemeralTest
waspDbStudioEphemeralTest =
  makeEphemeralTest
    "wasp-db-studio"
    [ makeEphemeralTestCase
        "Should fail outside of a Wasp project"
        (return "! wasp-cli db studio"),
      makeEphemeralTestCase
        "Setup: Create Wasp project from minimal starter"
        (createEphemeralWaspProject Minimal)
      -- questions: 
      -- how do we run db non-interactively?
      -- how do we know when the db is started?
      -- how do we stop the process?
    ]