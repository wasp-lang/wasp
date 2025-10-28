module Tests.WaspDockerfileEphemeralTest (waspDockerfileEphemeralTest) where

import EphemeralTest (EphemeralTest, makeEphemeralTest)
import EphemeralTest.ShellCommands (createEphemeralWaspProjectFromMinimalStarter, withInEphemeralWaspProjectDir)
import ShellCommands (writeToStdErrOnFailureAndExit, writeToStdErrOnSuccessAndExit)
import WaspProject.ShellCommands (waspCliDockerfile)

waspDockerfileEphemeralTest :: EphemeralTest
waspDockerfileEphemeralTest =
  makeEphemeralTest
    "wasp-dockerfile"
    [ writeToStdErrOnSuccessAndExit
        (return "wasp-cli dockerfile") -- we use a string here because `WaspProject.ShellCommands.waspCliDockerfile` has a 'WaspProject.ShellCommands.WaspProjectContext' requirement
        "Wasp dockerfile returned success outside of a Wasp project",
      createEphemeralWaspProjectFromMinimalStarter,
      withInEphemeralWaspProjectDir
        [ writeToStdErrOnFailureAndExit
            waspCliDockerfile
            "Wasp dockerfile failed inside of a Wasp project"
            -- NOTE: we don't test dockerfile contents
        ]
    ]
