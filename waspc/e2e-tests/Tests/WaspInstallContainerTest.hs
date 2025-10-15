module Tests.WaspInstallContainerTest (waspInstallContainerTest) where

import ContainerTest (ContainerTest, makeContainerTest)
import ContainerTest.ShellCommands
  ( ContainerTestContext,
  )
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    (~|),
  )
import qualified Wasp.SemanticVersion as SV

-- | Note that Wasp install script lives outside of Wasp CLI,
-- We should not export functions defined here outside of this module.
waspInstallContainerTest :: ContainerTest
waspInstallContainerTest =
  makeContainerTest
    "wasp-install"
    [ installWaspCli $ SV.Version 0 17 0,
      assertWaspCliVerionDirectoryExists $ SV.Version 0 17 0,
      installLatestWaspCLi,
      assertLatestWaspCliVerionDirectoryExists,
      assertWaspCliVerionDirectoryExists $ SV.Version 0 17 0, -- we didn't overwrite the old version
      (~| "grep -q 'Found an existing installation on the disk'") <$> installWaspCli (SV.Version 0 17 0)
    ]
  where
    installLatestWaspCLi :: ShellCommandBuilder ContainerTestContext ShellCommand
    installLatestWaspCLi = return "curl -sSL https://get.wasp.sh/installer.sh | sh -s"

    installWaspCli :: SV.Version -> ShellCommandBuilder ContainerTestContext ShellCommand
    installWaspCli version =
      return $ "curl -sSL https://get.wasp.sh/installer.sh | sh -s -- -v " ++ show version

    assertLatestWaspCliVerionDirectoryExists :: ShellCommandBuilder context ShellCommand
    assertLatestWaspCliVerionDirectoryExists = return $ "[ -d ~/.local/share/wasp-lang/$(" ++ curlLatestWaspVersion ++ ") ]"
      where
        -- Copied from https://github.com/wasp-lang/get-wasp-sh/blob/994a3f0817aa0d7b1b3001f504a6bad781393da2/installer.sh#L292
        curlLatestWaspVersion = "curl -LIs -o /dev/null -w '%{url_effective}' https://github.com/wasp-lang/wasp/releases/latest | awk -F/ '{print $NF}' | cut -c2-"

    assertWaspCliVerionDirectoryExists :: SV.Version -> ShellCommandBuilder context ShellCommand
    assertWaspCliVerionDirectoryExists version = return $ "[ -d ~/.local/share/wasp-lang/" ++ show version ++ " ]"
