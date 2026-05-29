module Tests.WaspInstallTest (waspInstallTest) where

import ShellCommands
  ( ShellCommand,
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
                  return $ assertSymlinkExists "node_modules/@wasp.sh/spec",
                  waspCliCompile
                ]
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
