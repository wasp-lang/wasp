module Test.WaspCleanTest (waspCleanTest) where

import ShellCommands (ShellCommand, WaspNewTemplate (..))
import Test (Test, makeTest, makeTestCase)
import Test.ShellCommands (createTestWaspProject, withInTestWaspProjectDir)
import WaspProject.ShellCommands (waspCliClean, waspCliCompile)

waspCleanTest :: Test
waspCleanTest =
  makeTest
    "wasp-clean"
    [ makeTestCase
        "Should fail outside of a Wasp project"
        (return [waspCliCleanFails]),
      makeTestCase
        "Should succeed inside of a uncompiled Wasp project"
        ( sequence
            [ createTestWaspProject Minimal,
              withInTestWaspProjectDir
                [ waspCliClean,
                  return $ assertDirectoryDoesNotExist ".wasp",
                  return $ assertDirectoryDoesNotExist "node_modules"
                ]
            ]
        ),
      makeTestCase
        "Should succeed inside of a compiled Wasp project"
        ( sequence
            [ createTestWaspProject Minimal,
              withInTestWaspProjectDir
                [ waspCliCompile,
                  waspCliClean,
                  return $ assertDirectoryDoesNotExist ".wasp",
                  return $ assertDirectoryDoesNotExist "node_modules"
                ]
            ]
        )
    ]
  where
    waspCliCleanFails :: ShellCommand
    waspCliCleanFails = "! wasp-cli clean"

    assertDirectoryDoesNotExist :: FilePath -> ShellCommand
    assertDirectoryDoesNotExist dirFilePath = "[ ! -d '" ++ dirFilePath ++ "' ]"
