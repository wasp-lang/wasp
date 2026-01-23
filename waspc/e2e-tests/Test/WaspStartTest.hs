module Test.WaspStartTest (waspStartTest) where

import ShellCommands (ShellCommand, ShellCommandBuilder, WaspNewTemplate (..))
import Test (Test, makeTest, makeTestCase)
import Test.ShellCommands (createE2eWaspProject, withInE2eWaspProjectDir)
import WaspProject.ShellCommands (waspCliStart)

-- FIXME: @waspCliStart@ - figure out long lasting processes
waspStartTest :: Test
waspStartTest =
  makeTest
    "wasp-start"
    [ makeTestCase
        "Should fail outside of a Wasp project"
        waspCliStartFails,
      makeTestCase
        "Setup: Create Wasp project from minimal starter"
        (createE2eWaspProject Minimal),
      makeTestCase
        "Should succeed inside of a uncompiled Wasp project"
        (withInE2eWaspProjectDir [waspCliStart]),
      makeTestCase
        "Assert `.wasp` directory exists"
        (withInE2eWaspProjectDir [assertDirectoryExists ".wasp"]),
      makeTestCase
        "Assert `node_modules` directory exists"
        (withInE2eWaspProjectDir [assertDirectoryExists "node_modules"]),
      makeTestCase
        "Should succeed inside of a compiled Wasp project"
        (withInE2eWaspProjectDir [waspCliStart])
    ]
  where
    waspCliStartFails :: ShellCommandBuilder context ShellCommand
    waspCliStartFails = return "! wasp-cli start"

    assertDirectoryExists :: FilePath -> ShellCommandBuilder context ShellCommand
    assertDirectoryExists dirFilePath = return $ "[ -d '" ++ dirFilePath ++ "' ]"
