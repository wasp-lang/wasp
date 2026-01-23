module Test.WaspCompileTest (waspCompileTest) where

import ShellCommands (ShellCommand, ShellCommandBuilder, WaspNewTemplate (..))
import Test (Test, makeTest, makeTestCase)
import Test.ShellCommands (createTestWaspProject, withInTestWaspProjectDir)
import WaspProject.ShellCommands (waspCliCompile)

waspCompileTest :: Test
waspCompileTest =
  makeTest
    "wasp-compile"
    [ makeTestCase
        "Should fail outside of a Wasp project"
        waspCliCompileFails,
      makeTestCase
        "Setup: Create Wasp project from minimal starter"
        (createTestWaspProject Minimal),
      makeTestCase
        "Should succeed inside of a uncompiled Wasp project"
        (withInTestWaspProjectDir [waspCliCompile]),
      makeTestCase
        "Assert `.wasp` directory exists"
        (withInTestWaspProjectDir [assertDirectoryExists ".wasp"]),
      makeTestCase
        "Assert `node_modules` directory exists"
        (withInTestWaspProjectDir [assertDirectoryExists "node_modules"]),
      makeTestCase
        "Should succeed inside of a compiled Wasp project"
        (withInTestWaspProjectDir [waspCliCompile]),
      makeTestCase
        "Assert `.wasp` directory exists"
        (withInTestWaspProjectDir [assertDirectoryExists ".wasp"]),
      makeTestCase
        "Assert `node_modules` directory exists"
        (withInTestWaspProjectDir [assertDirectoryExists "node_modules"])
    ]
  where
    waspCliCompileFails :: ShellCommandBuilder context ShellCommand
    waspCliCompileFails = return "! wasp-cli compile"

    assertDirectoryExists :: FilePath -> ShellCommandBuilder context ShellCommand
    assertDirectoryExists dirFilePath = return $ "[ -d '" ++ dirFilePath ++ "' ]"
