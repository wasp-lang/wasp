module EphemeralTest.WaspDockerfileEphemeralTest (waspDockerfileEphemeralTest) where

import EphemeralTest (EphemeralTest, makeEphemeralTest, makeEphemeralTestCase)
import EphemeralTest.ShellCommands (createEphemeralWaspProjectFromMinimalStarter, withInEphemeralWaspProjectDir)
import WaspProject.ShellCommands (waspCliDockerfile)

waspDockerfileEphemeralTest :: EphemeralTest
waspDockerfileEphemeralTest =
  makeEphemeralTest
    "wasp-dockerfile"
    [ makeEphemeralTestCase
        "Should fail outside of a Wasp project"
        (return "! wasp-cli dockerfile"), -- we use a string here because `WaspProject.ShellCommands.waspCliDockerfile` has a 'WaspProject.ShellCommands.WaspProjectContext' requirement
      makeEphemeralTestCase
        "Setup: Create Wasp project from minimal starter"
        createEphemeralWaspProjectFromMinimalStarter,
      makeEphemeralTestCase
        "Should succeed inside of a Wasp project"
        (withInEphemeralWaspProjectDir [waspCliDockerfile])
        -- NOTE: we don't test dockerfile contents
    ]
