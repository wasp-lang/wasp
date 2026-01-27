module Tests.WaspCompileTest (waspCompileTest) where

import ShellCommands (ShellCommand, WaspNewTemplate (..))
import Test (Test (..), TestCase (..))
import Test.ShellCommands (createTestWaspProject, inTestWaspProjectDir)
import WaspProject.ShellCommands (waspCliCompile)

waspCompileTest :: Test
waspCompileTest =
  Test
    "wasp-compile"
    [ TestCase
        "fail-outside-project"
        (return [waspCliCompileFails]),
      TestCase
        "succeed-uncompiled-project"
        ( sequence
            [ createTestWaspProject Minimal,
              inTestWaspProjectDir
                [ waspCliCompile,
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
