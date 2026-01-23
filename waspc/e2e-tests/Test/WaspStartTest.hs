module Test.WaspStartTest (waspStartTest) where

import ShellCommands (ShellCommand, ShellCommandBuilder, WaspNewTemplate (..))
import Test (Test, makeTest, makeTestCase)
import Test.ShellCommands (createTestWaspProject, withInTestWaspProjectDir)
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
        (createTestWaspProject Minimal),
      makeTestCase
        "Should succeed inside of a uncompiled Wasp project"
        (withInTestWaspProjectDir [waspCliStart]),
      makeTestCase
        "Assert `.wasp` directory exists"
        (withInTestWaspProjectDir [assertDirectoryExists ".wasp"]),
      makeTestCase
        "Assert `node_modules` directory exists"
        (withInTestWaspProjectDir [assertDirectoryExists "node_modules"]),
      makeTestCase
        "Should succeed inside of a compiled Wasp project"
        (withInTestWaspProjectDir [waspCliStart])
    ]
  where
    waspCliStartFails :: ShellCommandBuilder context ShellCommand
    waspCliStartFails = return "! wasp-cli start"

    assertDirectoryExists :: FilePath -> ShellCommandBuilder context ShellCommand
    assertDirectoryExists dirFilePath = return $ "[ -d '" ++ dirFilePath ++ "' ]"
