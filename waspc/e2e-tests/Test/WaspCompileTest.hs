module Test.WaspCompileTest (waspCompileTest) where

import ShellCommands (ShellCommand, ShellCommandBuilder, WaspNewTemplate (..))
import Test (Test, makeTest, makeTestCase)
import Test.ShellCommands (createE2eWaspProject, withInE2eWaspProjectDir)
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
        (createE2eWaspProject Minimal),
      makeTestCase
        "Should succeed inside of a uncompiled Wasp project"
        (withInE2eWaspProjectDir [waspCliCompile]),
      makeTestCase
        "Assert `.wasp` directory exists"
        (withInE2eWaspProjectDir [assertDirectoryExists ".wasp"]),
      makeTestCase
        "Assert `node_modules` directory exists"
        (withInE2eWaspProjectDir [assertDirectoryExists "node_modules"]),
      makeTestCase
        "Should succeed inside of a compiled Wasp project"
        (withInE2eWaspProjectDir [waspCliCompile]),
      makeTestCase
        "Assert `.wasp` directory exists"
        (withInE2eWaspProjectDir [assertDirectoryExists ".wasp"]),
      makeTestCase
        "Assert `node_modules` directory exists"
        (withInE2eWaspProjectDir [assertDirectoryExists "node_modules"])
    ]
  where
    waspCliCompileFails :: ShellCommandBuilder context ShellCommand
    waspCliCompileFails = return "! wasp-cli compile"

    assertDirectoryExists :: FilePath -> ShellCommandBuilder context ShellCommand
    assertDirectoryExists dirFilePath = return $ "[ -d '" ++ dirFilePath ++ "' ]"
