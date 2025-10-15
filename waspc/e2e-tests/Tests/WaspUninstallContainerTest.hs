module Tests.WaspUninstallContainerTest (waspUninstallContainerTest) where

import ContainerTest (ContainerTest, makeContainerTest)
import ContainerTest.ShellCommands (unisntallWaspCli)
import ShellCommands

waspUninstallContainerTest :: ContainerTest
waspUninstallContainerTest =
  makeContainerTest
    "wasp-uninstall"
    [ -- Install other Wasp versions to test the complete uninstall?
      -- But how to get back to development version of Wasp then?
      unisntallWaspCli,
      assertDirectoryMissing "~/.local/share/wasp-lang",
      assertFileMissing "~/.local/bin/wasp"
    ]

assertFileMissing :: String -> ShellCommandBuilder context ShellCommand
assertFileMissing directory = return $ "[ ! -f " ++ directory ++ " ]"

assertDirectoryMissing :: String -> ShellCommandBuilder context ShellCommand
assertDirectoryMissing directory = return $ "[ ! -d " ++ directory ++ " ]"
