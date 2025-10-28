module EphemeralTest.WaspInfoEphemeralTest (waspInfoEphemeralTest) where

import EphemeralTest (EphemeralTest, makeEphemeralTest)
import EphemeralTest.ShellCommands (createEphemeralWaspProjectFromMinimalStarter, withInEphemeralWaspProjectDir)
import ShellCommands (writeToStdErrOnFailureAndExit, writeToStdErrOnSuccessAndExit)
import WaspProject.ShellCommands (waspCliInfo)

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
            -- NOTE: we don't test for changing values like name, database, project dir size, and last compile
        ]
    ]
