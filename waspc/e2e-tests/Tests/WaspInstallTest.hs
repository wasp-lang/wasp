module Tests.WaspInstallTest (waspInstallTest) where

import ShellCommands
  ( ShellCommand,
    createTestWaspProject,
    inTestWaspProjectDir,
    waspCliClean,
    waspCliCompile,
    waspCliInstall,
    waspCliReinstall,
  )
import Test (Test (..), TestCase (..))
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (tsMinimalStarterTemplate)

waspInstallTest :: Test
waspInstallTest =
  Test
    "wasp-install"
    [ TestCase
        "install-fails-outside-project"
        (return [waspCliInstallFails]),
      TestCase
        "reinstall-fails-outside-project"
        (return [waspCliReinstallFails]),
      TestCase
        "install-restores-wasp-config-after-clean"
        ( sequence
            [ createTestWaspProject tsMinimalStarterTemplate,
              inTestWaspProjectDir
                [ waspCliClean,
                  return $ assertDirectoryDoesNotExist "node_modules",
                  waspCliInstall,
                  return $ assertSymlinkExists "node_modules/wasp-config",
                  waspCliCompile
                ]
            ]
        ),
      TestCase
        "reinstall-ensures-wasp-config"
        ( sequence
            [ createTestWaspProject tsMinimalStarterTemplate,
              inTestWaspProjectDir
                [ waspCliReinstall,
                  return $ assertSymlinkExists "node_modules/wasp-config",
                  waspCliCompile
                ]
            ]
        )
    ]
  where
    waspCliInstallFails :: ShellCommand
    waspCliInstallFails = "! wasp-cli install"

    waspCliReinstallFails :: ShellCommand
    waspCliReinstallFails = "! wasp-cli reinstall"

    assertDirectoryDoesNotExist :: FilePath -> ShellCommand
    assertDirectoryDoesNotExist dirFilePath = "[ ! -d '" ++ dirFilePath ++ "' ]"

    assertSymlinkExists :: FilePath -> ShellCommand
    assertSymlinkExists path = "[ -L '" ++ path ++ "' ]"
