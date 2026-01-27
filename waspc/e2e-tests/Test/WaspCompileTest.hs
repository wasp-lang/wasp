module Test.WaspCompileTest (waspCompileTest) where

import ShellCommands (ShellCommand, WaspNewTemplate (..))
import Test (Test, makeTest, makeTestCase)
import Test.ShellCommands (createTestWaspProject, withInTestWaspProjectDir)
import WaspProject.ShellCommands (waspCliCompile)

waspCompileTest :: Test
waspCompileTest =
  makeTest
    "wasp-compile"
    [ makeTestCase
        "Should fail outside of a Wasp project"
        (return [waspCliCompileFails]),
      makeTestCase
        "Should succeed inside of a uncompiled Wasp project"
        ( sequence
            [ createTestWaspProject Minimal,
              withInTestWaspProjectDir
                [ waspCliCompile,
                  return $ assertDirectoryExists ".wasp",
                  return $ assertDirectoryExists "node_modules"
                ]
            ]
        ),
      makeTestCase
        "Should succeed inside of a compiled Wasp project"
        ( sequence
            [ createTestWaspProject Minimal,
              withInTestWaspProjectDir
                [ waspCliCompile,
                  waspCliCompile,
                  return $ assertDirectoryExists ".wasp",
                  return $ assertDirectoryExists "node_modules"
                ]
            ]
        )
    ]
  where
    waspCliCompileFails :: ShellCommand
    waspCliCompileFails = "! wasp-cli compile"

    assertDirectoryExists :: FilePath -> ShellCommand
    assertDirectoryExists dirFilePath = "[ -d '" ++ dirFilePath ++ "' ]"
