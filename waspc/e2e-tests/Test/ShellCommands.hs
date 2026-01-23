{-# LANGUAGE OverloadedRecordDot #-}

module Test.ShellCommands
  ( TestContext (..),
    withInTestWaspProjectDir,
    createTestWaspProject,
  )
where

import Control.Monad.Reader (MonadReader (ask))
import ShellCommands (ShellCommand, ShellCommandBuilder, WaspNewTemplate, buildShellCommand, waspCliNew, (~&&))
import StrongPath (Abs, Dir, Path', fromAbsDir)
import Test.FileSystem (TestDir)
import WaspProject.ShellCommands (WaspProjectContext (..))

data TestContext = TestContext
  { testDir :: Path' Abs (Dir TestDir),
    waspProjectContext :: WaspProjectContext
  }

withInTestWaspProjectDir ::
  [ShellCommandBuilder WaspProjectContext ShellCommand] ->
  ShellCommandBuilder TestContext ShellCommand
withInTestWaspProjectDir waspProjectCommandBuilders = do
  testContext <- ask
  return $
    unwords ["cd", fromAbsDir (_waspProjectDir testContext.waspProjectContext)]
      ~&& foldr1 (~&&) (testWaspProjectCommands testContext)
      ~&& unwords ["cd", fromAbsDir testContext.testDir]
  where
    testWaspProjectCommands :: TestContext -> [ShellCommand]
    testWaspProjectCommands testContext =
      buildShellCommand testContext.waspProjectContext $ sequence waspProjectCommandBuilders

createTestWaspProject :: WaspNewTemplate -> ShellCommandBuilder TestContext ShellCommand
createTestWaspProject template = do
  testContext <- ask
  waspCliNew (_waspProjectName testContext.waspProjectContext) template
