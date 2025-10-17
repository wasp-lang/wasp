module Tests.WaspUninstallContainerTest (waspUninstallContainerTest) where

import ContainerTest (ContainerTest, makeContainerTest)
import ContainerTest.ShellCommands (unisntallWaspCli)
import ShellCommands (ShellCommand, ShellCommandBuilder, writeToStdErrOnFailureAndExit)

waspUninstallContainerTest :: ContainerTest
waspUninstallContainerTest =
  makeContainerTest
    "wasp-uninstall"
    [ -- Install other Wasp versions to test the complete uninstall?
      -- But how to get back to development version of Wasp then?
      unisntallWaspCli,
      writeToStdErrOnFailureAndExit
        (assertDirectoryMissing "~/.local/share/wasp-lang")
        "Directory ~/.local/share/wasp-lang was not deleted by the uninstall command",
      writeToStdErrOnFailureAndExit
        (assertFileMissing "~/.local/bin/wasp")
        "File ~/.local/bin/wasp was not deleted by the uninstall command"
    ]
  where
    assertFileMissing :: String -> ShellCommandBuilder context ShellCommand
    assertFileMissing directory = return $ "[ ! -f " ++ directory ++ " ]"

    assertDirectoryMissing :: String -> ShellCommandBuilder context ShellCommand
    assertDirectoryMissing directory = return $ "[ ! -d " ++ directory ++ " ]"
