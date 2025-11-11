module EphemeralTest.WaspStartEphemeralTest (waspStartEphemeralTest) where

import EphemeralTest (EphemeralTest, makeEphemeralTest, makeEphemeralTestCase)
import EphemeralTest.ShellCommands (createEphemeralWaspProject, withInEphemeralWaspProjectDir)
import WaspProject.ShellCommands (waspCliStart)
import ShellCommands (WaspNewTemplate(..), ShellCommandBuilder, ShellCommand)

-- FIXME: @waspCliStart@ - figure out long lasting processes
waspStartEphemeralTest :: EphemeralTest
waspStartEphemeralTest =
  makeEphemeralTest
    "wasp-start"
    [ makeEphemeralTestCase
        "Should fail outside of a Wasp project"
        waspCliStartFails,
      makeEphemeralTestCase
        "Setup: Create Wasp project from minimal starter"
        (createEphemeralWaspProject Minimal),
      makeEphemeralTestCase
        "Should succeed inside of a uncompiled Wasp project"
        (withInEphemeralWaspProjectDir [waspCliStart]),
      makeEphemeralTestCase
        "Assert `.wasp` directory exists"
        (withInEphemeralWaspProjectDir [assertDirectoryExists ".wasp"]),
      makeEphemeralTestCase
        "Assert `node_modules` directory exists"
        (withInEphemeralWaspProjectDir [assertDirectoryExists "node_modules"]),
      makeEphemeralTestCase
        "Should succeed inside of a compiled Wasp project"
        (withInEphemeralWaspProjectDir [waspCliStart])
    ]
    where
      waspCliStartFails :: ShellCommandBuilder context ShellCommand
      waspCliStartFails = return "! wasp-cli start"

      assertDirectoryExists :: FilePath -> ShellCommandBuilder context ShellCommand
      assertDirectoryExists dirFilePath = return $ "[ -d '" ++ dirFilePath ++ "' ]"