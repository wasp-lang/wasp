module Tests.WaspCleanTest (waspCleanTest) where

import ShellCommands (ShellCommand, WaspNewTemplate (..), createTestWaspProject, inTestWaspProjectDir, waspCliClean, waspCliCompile)
import Test (Test (..), TestCase (..))

waspCleanTest :: Test
waspCleanTest =
  Test
    "wasp-clean"
    [ TestCase
        "fail-outside-project"
        (return [waspCliCleanFails]),
      TestCase
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
      TestCase
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
