module EphemeralTest.WaspCleanEphemeralTest (waspCleanEphemeralTest) where

import EphemeralTest (EphemeralTest, makeEphemeralTest, makeEphemeralTestCase)
import EphemeralTest.ShellCommands (createEphemeralWaspProject, withInEphemeralWaspProjectDir)
import WaspProject.ShellCommands (waspCliClean, waspCliCompile)
import ShellCommands (WaspNewTemplate(..), ShellCommandBuilder, ShellCommand)

waspCleanEphemeralTest :: EphemeralTest
waspCleanEphemeralTest =
  makeEphemeralTest
    "wasp-clean"
    [ makeEphemeralTestCase
        "Should fail outside of a Wasp project"
        waspCliCleanFails,
      makeEphemeralTestCase
        "Setup: Create Wasp project from minimal starter"
        (createEphemeralWaspProject Minimal),
      makeEphemeralTestCase
        "Should succeed inside of a uncompiled Wasp project"
        (withInEphemeralWaspProjectDir [waspCliClean]),
      makeEphemeralTestCase
        "Setup: compile the Wasp project"
        (withInEphemeralWaspProjectDir [waspCliCompile]),
      makeEphemeralTestCase
        "Should succeed inside of a compiled Wasp project"
        (withInEphemeralWaspProjectDir [waspCliClean]),
      makeEphemeralTestCase
        "Assert `.wasp` directory does not exist"
        (withInEphemeralWaspProjectDir [assertDirectoryDoesNotExist ".wasp"]),
      makeEphemeralTestCase
        "Assert `node_modules` directory does not exist"
        (withInEphemeralWaspProjectDir [assertDirectoryDoesNotExist "node_modules"])
    ]
    where
      waspCliCleanFails :: ShellCommandBuilder context ShellCommand
      waspCliCleanFails = return "! wasp-cli clean"

      assertDirectoryDoesNotExist :: FilePath -> ShellCommandBuilder context ShellCommand
      assertDirectoryDoesNotExist dirFilePath = return $ "[ ! -d '" ++ dirFilePath ++ "' ]"