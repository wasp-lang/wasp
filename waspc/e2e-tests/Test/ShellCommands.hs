module Test.ShellCommands
  ( E2eTestContext (..),
    withInE2eWaspProjectDir,
    createE2eWaspProject,
  )
where

import Control.Monad.Reader (MonadReader (ask))
import ShellCommands (ShellCommand, ShellCommandBuilder, WaspNewTemplate, buildShellCommand, waspCliNew, (~&&))
import StrongPath (Abs, Dir, Path', fromAbsDir)
import Test.FileSystem (TestDir)
import WaspProject.ShellCommands (WaspProjectContext (..))

data E2eTestContext = E2eTestContext
  { _e2eDir :: Path' Abs (Dir TestDir),
    _e2eWaspProjectContext :: WaspProjectContext
  }

withInE2eWaspProjectDir ::
  [ShellCommandBuilder WaspProjectContext ShellCommand] ->
  ShellCommandBuilder E2eTestContext ShellCommand
withInE2eWaspProjectDir waspProjectCommandBuilders = do
  e2eTestContext <- ask
  return $
    unwords ["cd", fromAbsDir (_waspProjectDir $ _e2eWaspProjectContext e2eTestContext)]
      ~&& foldr1 (~&&) (e2eWaspProjectCommands e2eTestContext)
      ~&& unwords ["cd", fromAbsDir (_e2eDir e2eTestContext)]
  where
    e2eWaspProjectCommands :: E2eTestContext -> [ShellCommand]
    e2eWaspProjectCommands e2eTestContext =
      buildShellCommand (_e2eWaspProjectContext e2eTestContext) $ sequence waspProjectCommandBuilders

createE2eWaspProject :: WaspNewTemplate -> ShellCommandBuilder E2eTestContext ShellCommand
createE2eWaspProject template = do
  e2eTestContext <- ask
  waspCliNew (_waspProjectName $ _e2eWaspProjectContext e2eTestContext) template
