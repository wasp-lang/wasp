module Test.WaspStartTest (waspStartTest) where

import ShellCommands (ShellCommand, WaspNewTemplate (..))
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
        (return [waspCliStartFails]),
      makeTestCase
        "Should succeed inside of a uncompiled Wasp project"
        ( sequence
            [ createTestWaspProject Minimal,
              withInTestWaspProjectDir
                [ waspCliStart,
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
                [ waspCliStart,
                  waspCliStart
                ]
            ]
        )
    ]
  where
    waspCliStartFails :: ShellCommand
    waspCliStartFails = "! wasp-cli start"

    assertDirectoryExists :: FilePath -> ShellCommand
    assertDirectoryExists dirFilePath = "[ -d '" ++ dirFilePath ++ "' ]"
