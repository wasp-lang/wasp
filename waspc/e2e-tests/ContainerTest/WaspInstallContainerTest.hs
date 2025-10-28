module ContainerTest.WaspInstallContainerTest (waspInstallContainerTest) where

import ContainerTest (ContainerTest, makeContainerTest)
import ContainerTest.ShellCommands
  ( installLatestWaspCli,
    installWaspCliVersion,
  )
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    writeToStdErrOnFailureAndExit,
    (~|),
  )
import StrongPath (Abs, Dir, Path', fromAbsDir, (</>))
import System.Process (readCreateProcess, shell)
import Wasp.Cli.FileSystem
  ( UserHomeDir,
    getHomeDir,
    waspInstallationDirInHomeDir,
  )
import qualified Wasp.SemanticVersion.Version as SV

-- | Note that Wasp install script lives outside of Wasp CLI,
-- We should not export functions defined here outside of this module.
waspInstallContainerTest :: IO ContainerTest
waspInstallContainerTest = do
  homeDir <- getHomeDir
  latestWaspVersion <- readCreateProcess (shell curlLatestWaspVersion) ""
  return $ makeWaspInstallContainerTest homeDir latestWaspVersion
  where
    -- Copied from https://github.com/wasp-lang/get-wasp-sh/blob/994a3f0817aa0d7b1b3001f504a6bad781393da2/installer.sh#L292
    curlLatestWaspVersion :: ShellCommand
    curlLatestWaspVersion = "curl -LIs -o /dev/null -w '%{url_effective}' https://github.com/wasp-lang/wasp/releases/latest | awk -F/ '{print $NF}' | cut -c2-"

    makeWaspInstallContainerTest :: Path' Abs (Dir UserHomeDir) -> String -> ContainerTest
    makeWaspInstallContainerTest homeDir latestWaspVersion =
      makeContainerTest
        "wasp-install"
        [ writeToStdErrOnFailureAndExit
            (installWaspCliVersion specificWaspVersion)
            "Installer should suppoert installing a specific Wasp version",
          writeToStdErrOnFailureAndExit
            (assertWaspCliVerionDirectoryExists specificWaspVersion)
            ("Installing Wasp version should create the " ++ fromAbsDir waspInstallationDir ++ "/<version> directory"),
          installLatestWaspCli,
          writeToStdErrOnFailureAndExit
            assertLatestWaspCliVerionDirectoryExists
            ("Installing latest Wasp version should create the " ++ fromAbsDir waspInstallationDir ++ "/<version> directory"),
          writeToStdErrOnFailureAndExit
            (assertWaspCliVerionDirectoryExists specificWaspVersion)
            "Installing multiple Wasp versions should not remove the previously installed versions directories",
          writeToStdErrOnFailureAndExit
            -- This only tests that the edge case is recognized, it doesn't test the actual reuse/download.
            ((~| "grep -q 'Found an existing installation on the disk'") <$> installWaspCliVersion specificWaspVersion)
            "Installing already installed version should reuse the existing installation on the disk"
        ]
      where
        assertLatestWaspCliVerionDirectoryExists :: ShellCommandBuilder context ShellCommand
        assertLatestWaspCliVerionDirectoryExists = return $ "[ -d " ++ fromAbsDir waspInstallationDir ++ latestWaspVersion ++ " ]"

        assertWaspCliVerionDirectoryExists :: SV.Version -> ShellCommandBuilder context ShellCommand
        assertWaspCliVerionDirectoryExists version = return $ "[ -d " ++ fromAbsDir waspInstallationDir ++ show version ++ " ]"

        waspInstallationDir = homeDir </> waspInstallationDirInHomeDir
        specificWaspVersion = SV.Version 0 17 0
