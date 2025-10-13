module ContainerTest.ShellCommands
  ( ContainerTestContext (..),
    installLatestWaspCLi,
    installWaspCli,
    assertFoundExistingInstallation,
    unisntallWaspCli,
  )
where

import ShellCommands (ShellCommand, ShellCommandBuilder, (~|))
import qualified Wasp.SemanticVersion as SV

-- | Shell commands that must run inside a container.
-- They can modify the host OS environment, so we isolate them in containers.
data ContainerTestContext = ContainerTestContext {}

installLatestWaspCLi :: ShellCommandBuilder ContainerTestContext ShellCommand
installLatestWaspCLi = return "curl -sSL https://get.wasp.sh/installer.sh | sh -s"

-- | Installs a specific version of the Wasp CLI.
installWaspCli :: SV.Version -> ShellCommandBuilder ContainerTestContext ShellCommand
installWaspCli version =
  return $ "curl -sSL https://get.wasp.sh/installer.sh | sh -s -- -v " ++ show version

-- | Asserts that we are switching to an already existing version, 
-- rather than installing it again.
assertFoundExistingInstallation :: SV.Version -> ShellCommandBuilder ContainerTestContext ShellCommand
assertFoundExistingInstallation version =
  return $
    "curl -sSL https://get.wasp.sh/installer.sh | sh -s -- -v " ++ show version
      ~| "grep -q \"Found an existing installation on the disk\""

-- | Uninstalls all installed Wasp CLI files.
-- This includes all other installed versions.
unisntallWaspCli :: ShellCommandBuilder ContainerTestContext ShellCommand
unisntallWaspCli = return $ "yes" ~| "wasp uninstall"
