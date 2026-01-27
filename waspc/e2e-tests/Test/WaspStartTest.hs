module Test.WaspStartTest (waspStartTest) where

import ShellCommands (ShellCommand, WaspNewTemplate (..))
import Test (Test, makeTest, makeTestCase)
import Test.ShellCommands (createTestWaspProject, withInTestWaspProjectDir)
import WaspProject.ShellCommands (waspCliCompile, waspCliStart)

-- FIXME: @waspCliStart@ - figure out long lasting processes
waspStartTest :: Test
waspStartTest =
  makeTest
    "wasp-start"
    [ makeTestCase
        "fail-outside-project"
        (return [waspCliStartFails]),
      makeTestCase
        "succeed-uncompiled-project"
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
        "succeed-compiled-project"
        ( sequence
            [ createTestWaspProject Minimal,
              withInTestWaspProjectDir
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
