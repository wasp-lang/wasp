module EphemeralTest.WaspInfoEphemeralTest (waspInfoEphemeralTest) where

import EphemeralTest (EphemeralTest, makeEphemeralTest)
import EphemeralTest.ShellCommands (createEphemeralWaspProjectFromMinimalStarter, withInEphemeralWaspProjectDir)
import ShellCommands (writeToStdErrOnFailureAndExit, writeToStdErrOnSuccessAndExit)
import WaspProject.ShellCommands (waspCliInfo)

-- TODO: test values changing: name, database, project dir size, last compile
waspInfoEphemeralTest :: EphemeralTest
waspInfoEphemeralTest =
  makeEphemeralTest
    "wasp-info"
    [ writeToStdErrOnSuccessAndExit
        (return "wasp-cli info") -- we use a string here because `WaspProject.ShellCommands.waspCliInfo` has a 'WaspProject.ShellCommands.WaspProjectContext' requirement
        "Wasp info returned success outside of a Wasp project",
      createEphemeralWaspProjectFromMinimalStarter,
      withInEphemeralWaspProjectDir
        [ writeToStdErrOnFailureAndExit
            waspCliInfo
            "Wasp info failed inside of a Wasp project"
        ]
    ]
