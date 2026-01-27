module Tests.WaspCleanTest (waspCleanTest) where

import ShellCommands (ShellCommand, WaspNewTemplate (..))
import Test (Test, makeTest, makeTestCase)
import Test.ShellCommands (createTestWaspProject, inTestWaspProjectDir)
import WaspProject.ShellCommands (waspCliClean, waspCliCompile)

waspCleanTest :: Test
waspCleanTest =
  makeTest
    "wasp-clean"
    [ makeTestCase
        "fail-outside-project"
        (return [waspCliCleanFails]),
      makeTestCase
        "succeed-uncompiled-project"
        ( sequence
            [ createTestWaspProject Minimal,
              inTestWaspProjectDir
                [ waspCliClean,
                  return $ assertDirectoryDoesNotExist ".wasp",
                  return $ assertDirectoryDoesNotExist "node_modules"
                ]
            ]
        ),
      makeTestCase
        "succeed-compiled-project"
        ( sequence
            [ createTestWaspProject Minimal,
              inTestWaspProjectDir
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
