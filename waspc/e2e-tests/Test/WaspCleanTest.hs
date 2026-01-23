module Test.WaspCleanTest (waspCleanTest) where

import ShellCommands (ShellCommand, ShellCommandBuilder, WaspNewTemplate (..))
import Test (Test, makeTest, makeTestCase)
import Test.ShellCommands (createTestWaspProject, withInTestWaspProjectDir)
import WaspProject.ShellCommands (waspCliClean, waspCliCompile)

waspCleanTest :: Test
waspCleanTest =
  makeTest
    "wasp-clean"
    [ makeTestCase
        "Should fail outside of a Wasp project"
        waspCliCleanFails,
      makeTestCase
        "Setup: Create Wasp project from minimal starter"
        (createTestWaspProject Minimal),
      makeTestCase
        "Should succeed inside of a uncompiled Wasp project"
        (withInTestWaspProjectDir [waspCliClean]),
      makeTestCase
        "Setup: compile the Wasp project"
        (withInTestWaspProjectDir [waspCliCompile]),
      makeTestCase
        "Should succeed inside of a compiled Wasp project"
        (withInTestWaspProjectDir [waspCliClean]),
      makeTestCase
        "Assert `.wasp` directory does not exist"
        (withInTestWaspProjectDir [assertDirectoryDoesNotExist ".wasp"]),
      makeTestCase
        "Assert `node_modules` directory does not exist"
        (withInTestWaspProjectDir [assertDirectoryDoesNotExist "node_modules"])
    ]
  where
    waspCliCleanFails :: ShellCommandBuilder context ShellCommand
    waspCliCleanFails = return "! wasp-cli clean"

    assertDirectoryDoesNotExist :: FilePath -> ShellCommandBuilder context ShellCommand
    assertDirectoryDoesNotExist dirFilePath = return $ "[ ! -d '" ++ dirFilePath ++ "' ]"
