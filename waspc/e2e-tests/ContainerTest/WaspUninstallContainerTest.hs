module ContainerTest.WaspUninstallContainerTest (waspUninstallContainerTest) where

import ContainerTest (ContainerTest, makeContainerTest)
import ContainerTest.ShellCommands (unisntallWaspCli)
import ShellCommands (ShellCommand, ShellCommandBuilder, writeToStdErrOnFailureAndExit)

-- TODO:
-- Force creation of telemetry data to test cache is deleted?
-- Install other Wasp versions to test the complete uninstall?
-- Currently we only copy the executable to the container -> can't test if installation dir was deleted properly?
waspUninstallContainerTest :: IO ContainerTest
waspUninstallContainerTest = 
  return $
    makeContainerTest
      "wasp-uninstall"
      [ unisntallWaspCli,
        writeToStdErrOnFailureAndExit
          (assertDirectoryMissing "\"$HOME/.local/share/wasp-lang\"")
          "Wasp installation directory should be deleted: \"$HOME/.local/share/wasp-lang\"",
        writeToStdErrOnFailureAndExit
          (assertDirectoryMissing "\"$HOME/.cache/wasp\"")
          "Wasp cache directory should be deleted: \"$HOME/.cache/wasp\"",
        writeToStdErrOnFailureAndExit
          (assertFileMissing "\"$HOME/.local/share/wasp-lang\"")
          "Wasp executable should be deleted: \"$HOME/.local/share/wasp-lang\""
      ]
  where
    assertFileMissing :: String -> ShellCommandBuilder context ShellCommand
    assertFileMissing directory = return $ "[ ! -f " ++ directory ++ " ]"

    assertDirectoryMissing :: String -> ShellCommandBuilder context ShellCommand
    assertDirectoryMissing directory = return $ "[ ! -d " ++ directory ++ " ]"