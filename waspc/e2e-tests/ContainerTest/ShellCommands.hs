module ContainerTest.ShellCommands
  ( ContainerTestContext (..),
    unisntallWaspCli,
    installWaspCliVersion,
    installLatestWaspCli,
  )
where

import ShellCommands (ShellCommand, ShellCommandBuilder, (~|))
import qualified Wasp.SemanticVersion as SV

-- | Shell commands that must run inside a container.
-- They can modify the host OS environment, so we isolate them in containers.
data ContainerTestContext = ContainerTestContext {}

-- | This includes all other installed versions.
unisntallWaspCli :: ShellCommandBuilder ContainerTestContext ShellCommand
unisntallWaspCli = return $ "yes" ~| "wasp-cli uninstall"

installLatestWaspCli :: ShellCommandBuilder ContainerTestContext ShellCommand
installLatestWaspCli = return "curl -sSL https://get.wasp.sh/installer.sh | sh -s"

installWaspCliVersion :: SV.Version -> ShellCommandBuilder ContainerTestContext ShellCommand
installWaspCliVersion version =
  return $ "curl -sSL https://get.wasp.sh/installer.sh | sh -s -- -v " ++ show version
