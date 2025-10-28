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
          ("Directory " ++ fromAbsDir waspInstallationDir ++ " was not deleted by the uninstall command"),
        writeToStdErrOnFailureAndExit
          (assertDirectoryMissing $ fromAbsDir waspCacheDir)
          ("Directory " ++ fromAbsDir waspCacheDir ++ " was not deleted by the uninstall command"),
        writeToStdErrOnFailureAndExit
          (assertFileMissing $ fromAbsFile waspExecutableFile)
          ("File " ++ fromAbsFile waspExecutableFile ++ " was not deleted by the uninstall command")
      ]
  where
    assertFileMissing :: String -> ShellCommandBuilder context ShellCommand
    assertFileMissing directory = return $ "[ ! -f " ++ directory ++ " ]"

    assertDirectoryMissing :: String -> ShellCommandBuilder context ShellCommand
    assertDirectoryMissing directory = return $ "[ ! -d " ++ directory ++ " ]"
