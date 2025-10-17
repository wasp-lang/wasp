module ContainerTest.ShellCommands
  ( ContainerTestContext (..),
    unisntallWaspCli,
  )
where

import ShellCommands (ShellCommand, ShellCommandBuilder, (~|))

-- | Shell commands that must run inside a container.
-- They can modify the host OS environment, so we isolate them in containers.
data ContainerTestContext = ContainerTestContext {}

-- | This includes all other installed versions.
unisntallWaspCli :: ShellCommandBuilder ContainerTestContext ShellCommand
unisntallWaspCli = return $ "yes" ~| "wasp-cli uninstall"
