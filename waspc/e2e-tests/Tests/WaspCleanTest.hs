module Tests.WaspCleanTest (waspCleanTest) where

import ShellCommands (ShellCommand, createTestWaspProject, inTestWaspProjectDir, waspCliClean, waspCliCompile)
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
        "preserves-state-unless-data-is-requested"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ waspCliCompile,
                  return "mkdir -p .wasp/state && touch .wasp/state/dev.db",
                  return "mkdir .wasp/unused",
                  waspCliClean,
                  return $ assertDirectoryDoesNotExist ".wasp/out",
                  return $ assertDirectoryDoesNotExist ".wasp/unused",
                  return "[ -f .wasp/state/dev.db ]",
                  return "$WASP_CLI_CMD clean --data",
                  return $ assertDirectoryDoesNotExist ".wasp"
                ]
            ]
        ),
      TestCase
        "rejects-unknown-arguments"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir [return "! $WASP_CLI_CMD clean --unknown"]
            ]
        )
    ]
  where
    waspCliCleanFails :: ShellCommand
    waspCliCleanFails = "! $WASP_CLI_CMD clean"

    assertDirectoryDoesNotExist :: FilePath -> ShellCommand
    assertDirectoryDoesNotExist dirFilePath = "[ ! -d '" ++ dirFilePath ++ "' ]"
