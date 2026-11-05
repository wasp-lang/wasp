module Tests.WaspInstallTest (waspInstallTest) where

import ShellCommands
  ( ShellCommand,
    assertCommandOutputContains,
    createTestWaspProject,
    inTestWaspProjectDir,
    waspCliClean,
    waspCliCompile,
    waspCliInstall,
  )
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)

waspInstallTest :: Test
waspInstallTest =
  Test
    "wasp-install"
    [ TestCase
        "install-fails-outside-project"
        (return [waspCliInstallFails]),
      TestCase
        "install-restores-wasp-spec-after-clean"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                [ waspCliClean,
                  return $ assertDirectoryDoesNotExist "node_modules",
                  waspCliInstall,
                  return $ assertSymlinkExists "node_modules/@wasp/spec",
                  waspCliCompile
                ]
            ]
        ),
      -- Unlike the other commands, `wasp install` shows npm's output as it comes.
      TestCase
        "shows-npm-output"
        ( sequence
            [ createTestWaspProject minimalStarterTemplate,
              inTestWaspProjectDir
                -- npm's install summary, e.g. "up to date, audited 359 packages in 2s".
                [assertCommandOutputContains waspCliInstall " packages"]
            ]
        )
    ]
  where
    waspCliInstallFails :: ShellCommand
    waspCliInstallFails = "! $WASP_CLI_CMD install"

    assertDirectoryDoesNotExist :: FilePath -> ShellCommand
    assertDirectoryDoesNotExist dirFilePath = "[ ! -d '" ++ dirFilePath ++ "' ]"

    assertSymlinkExists :: FilePath -> ShellCommand
    assertSymlinkExists path = "[ -L '" ++ path ++ "' ]"
