module EphemeralTest.WaspCompileEphemeralTest (waspCompileEphemeralTest) where

import EphemeralTest (EphemeralTest, makeEphemeralTest, makeEphemeralTestCase)
import EphemeralTest.ShellCommands (createEphemeralWaspProject, withInEphemeralWaspProjectDir)
import WaspProject.ShellCommands (waspCliCompile)
import ShellCommands (WaspNewTemplate(..), ShellCommandBuilder, ShellCommand)

waspCompileEphemeralTest :: EphemeralTest
waspCompileEphemeralTest =
  makeEphemeralTest
    "wasp-compile"
    [ makeEphemeralTestCase
        "Should fail outside of a Wasp project"
        waspCliCompileFails,
      makeEphemeralTestCase
        "Setup: Create Wasp project from minimal starter"
        (createEphemeralWaspProject Minimal),
      makeEphemeralTestCase
        "Should succeed inside of a uncompiled Wasp project"
        (withInEphemeralWaspProjectDir [waspCliCompile]),
      makeEphemeralTestCase
        "Should succeed inside of a compiled Wasp project"
        (withInEphemeralWaspProjectDir [waspCliCompile]),
      makeEphemeralTestCase
        "Assert `.wasp` directory exists"
        (withInEphemeralWaspProjectDir [assertDirectoryExists ".wasp"]),
      makeEphemeralTestCase
        "Assert `node_modules` directory exists"
        (withInEphemeralWaspProjectDir [assertDirectoryExists "node_modules"])
    ]
    where
      waspCliCompileFails :: ShellCommandBuilder context ShellCommand
      waspCliCompileFails = return "! wasp-cli compile"

      assertDirectoryExists :: FilePath -> ShellCommandBuilder context ShellCommand
      assertDirectoryExists dirFilePath = return $ "[ -d '" ++ dirFilePath ++ "' ]"