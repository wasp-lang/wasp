module Tests.WaspInstallUninstallContainerTest (waspInstallUninstallContainerTest) where

import ContainerTest (ContainerTest, makeContainerTest)
import ContainerTest.ShellCommands
  ( assertFoundExistingInstallation,
    installLatestWaspCLi,
    installWaspCli,
    unisntallWaspCli,
  )
import ShellCommands (ShellCommand, ShellCommandBuilder)
import qualified Wasp.SemanticVersion as SV

waspInstallUninstallContainerTest :: ContainerTest
waspInstallUninstallContainerTest =
  makeContainerTest
    "wasp-install-uninstall"
    [ installWaspCli $ SV.Version 0 17 0,
      assertWaspCliVersion $ SV.Version 0 17 0,
      assertDirectoryExists "~/.local/share/wasp-lang/0.17.0",
      assertFileExists "~/.local/bin/wasp",
      installLatestWaspCLi,
      -- how to get version to assert "wasp-cli verison" and files?
      assertFoundExistingInstallation $ SV.Version 0 17 0,
      assertWaspCliVersion $ SV.Version 0 17 0,
      unisntallWaspCli,
      assertDirectoryMissing "~/.local/share/wasp-lang",
      assertFileMissing "~/.local/bin/wasp"
    ]
  where
    assertWaspCliVersion :: SV.Version -> ShellCommandBuilder context ShellCommand
    assertWaspCliVersion version = return $ "[[ " ++ "$(wasp-cli version | head -n1)" ++ " == " ++ show version ++ " ]] || exit 1"

    assertFileMissing :: String -> ShellCommandBuilder context ShellCommand
    assertFileMissing directory = return $ "[ ! -f " ++ directory ++ " ]"

    assertFileExists :: String -> ShellCommandBuilder context ShellCommand
    assertFileExists directory = return $ "[ -f " ++ directory ++ " ]"

    assertDirectoryMissing :: String -> ShellCommandBuilder context ShellCommand
    assertDirectoryMissing directory = return $ "[ ! -d " ++ directory ++ " ]"

    assertDirectoryExists :: String -> ShellCommandBuilder context ShellCommand
    assertDirectoryExists directory = return $ "[ -d " ++ directory ++ " ]"