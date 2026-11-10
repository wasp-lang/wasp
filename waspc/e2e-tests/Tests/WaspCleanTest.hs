module Tests.WaspCleanTest (waspCleanTest) where

import ShellCommands (ShellCommand, createTestWaspProject, inTestWaspProjectDir, npmInstall, waspCliClean, waspCliCompile)
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

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
            [ createTestWaspProject minimalStarterTemplate,
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
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ waspCliCompile,
                  waspCliClean,
                  return $ assertDirectoryDoesNotExist ".wasp",
                  return $ assertDirectoryDoesNotExist "node_modules"
                ]
            ]
        ),
      TestCase
        "compile-succeeds-after-clean-and-npm-install"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ waspCliCompile,
                  waspCliClean,
                  npmInstall,
                  waspCliCompile
                ]
            ]
        )
    ]
  where
    waspCliCleanFails :: ShellCommand
    waspCliCleanFails = "! $WASP_CLI_CMD clean"

    assertDirectoryDoesNotExist :: FilePath -> ShellCommand
    assertDirectoryDoesNotExist dirFilePath = "[ ! -d '" ++ dirFilePath ++ "' ]"
