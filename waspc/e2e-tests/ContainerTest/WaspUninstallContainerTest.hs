module ContainerTest.WaspUninstallContainerTest (waspUninstallContainerTest) where

import ContainerTest (ContainerTest, makeContainerTest)
import ContainerTest.ShellCommands (unisntallWaspCli)
import ShellCommands (ShellCommand, ShellCommandBuilder, writeToStdErrOnFailureAndExit)
import StrongPath (fromAbsDir, fromAbsFile, (</>))
import Wasp.Cli.FileSystem
  ( getHomeDir,
    getUserCacheDir,
    getWaspCacheDir,
    waspExecutableInHomeDir,
    waspInstallationDirInHomeDir,
  )

waspUninstallContainerTest :: IO ContainerTest
waspUninstallContainerTest = do
  homeDir <- getHomeDir
  userCacheDir <- getUserCacheDir

  let waspInstallationDir = homeDir </> waspInstallationDirInHomeDir
      waspCacheDir = getWaspCacheDir userCacheDir
      waspExecutableFile = homeDir </> waspExecutableInHomeDir

  return $
    makeContainerTest
      "wasp-uninstall"
      [ -- TODO: Install other Wasp versions to test the complete uninstall?
        -- But how to get back to development version of Wasp then?
        unisntallWaspCli,
        writeToStdErrOnFailureAndExit
          (assertDirectoryMissing $ fromAbsDir waspInstallationDir)
          ("Wasp installation directory should be deleted: " ++ fromAbsDir waspInstallationDir),
        writeToStdErrOnFailureAndExit
          (assertDirectoryMissing $ fromAbsDir waspCacheDir)
          ("Wasp cache directory should be deleted: " ++ fromAbsDir waspCacheDir),
        writeToStdErrOnFailureAndExit
          (assertFileMissing $ fromAbsFile waspExecutableFile)
          ("Wasp executable should be deleted: " ++ fromAbsFile waspExecutableFile)
      ]
  where
    assertFileMissing :: String -> ShellCommandBuilder context ShellCommand
    assertFileMissing directory = return $ "[ ! -f " ++ directory ++ " ]"

    assertDirectoryMissing :: String -> ShellCommandBuilder context ShellCommand
    assertDirectoryMissing directory = return $ "[ ! -d " ++ directory ++ " ]"
