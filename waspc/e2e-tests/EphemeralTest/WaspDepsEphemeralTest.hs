module EphemeralTest.WaspDepsEphemeralTest (waspDepsEphemeralTest) where

import EphemeralTest (EphemeralTest, makeEphemeralTest)
import EphemeralTest.ShellCommands (createEphemeralWaspProjectFromMinimalStarter, withInEphemeralWaspProjectDir)
import ShellCommands (writeToStdErrOnFailureAndExit, writeToStdErrOnSuccessAndExit)
import WaspProject.ShellCommands (waspCliDockerfile)

-- TODO: test deps changing with installs/uninstalls
waspDepsEphemeralTest :: EphemeralTest
waspDepsEphemeralTest =
  makeEphemeralTest
    "wasp-deps"
    [ writeToStdErrOnSuccessAndExit
        (return "wasp-cli deps") -- we use a string here because `WaspProject.ShellCommands.waspCliDockerfile` has a 'WaspProject.ShellCommands.WaspProjectContext' requirement
        "'wasp deps' should fail outside of a Wasp project",
      createEphemeralWaspProjectFromMinimalStarter,
      withInEphemeralWaspProjectDir
        [ writeToStdErrOnFailureAndExit
            waspCliDockerfile
            "'wasp deps' should succeed insdie of a Wasp project"
        ]
    ]
