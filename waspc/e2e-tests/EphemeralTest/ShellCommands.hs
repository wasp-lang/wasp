module EphemeralTest.ShellCommands
  ( EphemeralTestContext (..),
    withInEphemeralWaspProjectDir,
    createEphemeralWaspProjectFromMinimalStarter,
  )
where

import Control.Monad.Reader (MonadReader (ask))
import EphemeralTest.FileSystem (EphemeralDir)
import ShellCommands (ShellCommand, ShellCommandBuilder, buildShellCommand, waspCliNewMinimalStarter, (~&&))
import StrongPath (Abs, Dir, Path', fromAbsDir)
import WaspProject.ShellCommands (WaspProjectContext (..))

data EphemeralTestContext = EphemeralTestContext
  { _ephemeralDir :: Path' Abs (Dir EphemeralDir),
    _ephemeralWaspProjectContext :: WaspProjectContext
  }

withInEphemeralWaspProjectDir ::
  [ShellCommandBuilder WaspProjectContext ShellCommand] ->
  ShellCommandBuilder EphemeralTestContext ShellCommand
withInEphemeralWaspProjectDir waspProjectCommandBuilders = do
  ephemeralTestContext <- ask
  return $
    "cd " ++ fromAbsDir (_waspProjectDir $ _ephemeralWaspProjectContext ephemeralTestContext)
      ~&& foldr1 (~&&) (ephemeralWaspProjectCommands ephemeralTestContext)
      ~&& "cd " ++ fromAbsDir (_ephemeralDir ephemeralTestContext)
  where
    ephemeralWaspProjectCommands :: EphemeralTestContext -> [ShellCommand]
    ephemeralWaspProjectCommands ephemeralTestContext =
      buildShellCommand (_ephemeralWaspProjectContext ephemeralTestContext) $ sequence waspProjectCommandBuilders

createEphemeralWaspProjectFromMinimalStarter :: ShellCommandBuilder EphemeralTestContext ShellCommand
createEphemeralWaspProjectFromMinimalStarter = do
  ephemeralTestContext <- ask
  waspCliNewMinimalStarter $ _waspProjectName $ _ephemeralWaspProjectContext ephemeralTestContext
