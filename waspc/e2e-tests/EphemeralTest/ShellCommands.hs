module EphemeralTest.ShellCommands
  ( EphemeralTestContext (..),
    withInEphemeralWaspProjectDir,
    createEphemeralWaspProject,
  )
where

import Control.Monad.Reader (MonadReader (ask))
import EphemeralTest.FileSystem (EphemeralDir)
import ShellCommands (ShellCommand, ShellCommandBuilder, WaspNewTemplate, buildShellCommand, waspCliNew, (~&&))
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
    "cd "
      ++ fromAbsDir (_waspProjectDir $ _ephemeralWaspProjectContext ephemeralTestContext)
        ~&& foldr1 (~&&) (ephemeralWaspProjectCommands ephemeralTestContext)
        ~&& "cd "
      ++ fromAbsDir (_ephemeralDir ephemeralTestContext)
  where
    ephemeralWaspProjectCommands :: EphemeralTestContext -> [ShellCommand]
    ephemeralWaspProjectCommands ephemeralTestContext =
      buildShellCommand (_ephemeralWaspProjectContext ephemeralTestContext) $ sequence waspProjectCommandBuilders

createEphemeralWaspProject :: WaspNewTemplate -> ShellCommandBuilder EphemeralTestContext ShellCommand
createEphemeralWaspProject template = do
  ephemeralTestContext <- ask
  waspCliNew (_waspProjectName $ _ephemeralWaspProjectContext ephemeralTestContext) template
