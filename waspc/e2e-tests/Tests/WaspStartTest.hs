module Tests.WaspStartTest (waspStartTest) where

import ShellCommands (ShellCommand, WaspNewTemplate (..), createTestWaspProject, inTestWaspProjectDir, waspCliCompile, waspCliStart)
import Test (Test (..), TestCase (..))

-- FIXME: @waspCliStart@ - figure out long lasting processes
waspStartTest :: Test
waspStartTest =
  Test
    "wasp-start"
    [ TestCase
        "fail-outside-project"
        (return [waspCliStartFails]),
      TestCase
        "succeed-uncompiled-project"
        ( sequence
            [ createTestWaspProject Minimal,
              inTestWaspProjectDir
                [ waspCliStart,
                  return $ assertDirectoryExists ".wasp",
                  return $ assertDirectoryExists "node_modules"
                ]
            ]
        ),
      TestCase
        "succeed-compiled-project"
        ( sequence
            [ createTestWaspProject Minimal,
              inTestWaspProjectDir
                [ waspCliCompile,
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
