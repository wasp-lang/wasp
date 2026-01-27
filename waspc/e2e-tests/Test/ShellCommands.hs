{-# LANGUAGE OverloadedRecordDot #-}

module Test.ShellCommands
  ( TestContext (..),
    inTestWaspProjectDir,
    createTestWaspProject,
  )
where

import Control.Monad.Reader (MonadReader (ask))
import ShellCommands (ShellCommand, ShellCommandBuilder, WaspNewTemplate, buildShellCommand, waspCliNew, (~&&))
import StrongPath (Abs, Dir, Path', fromAbsDir)
import Test.FileSystem (TestCaseDir)
import WaspProject.ShellCommands (WaspProjectContext (..))

data TestContext = TestContext
  { testCaseDir :: Path' Abs (Dir TestCaseDir),
    waspProjectContext :: WaspProjectContext
  }

inTestWaspProjectDir ::
  [ShellCommandBuilder WaspProjectContext ShellCommand] ->
  ShellCommandBuilder TestContext ShellCommand
inTestWaspProjectDir waspProjectCommandBuilders = do
  testContext <- ask
  return $
    unwords ["cd", fromAbsDir (_waspProjectDir testContext.waspProjectContext)]
      ~&& foldr1 (~&&) (buildShellCommand testContext.waspProjectContext $ sequence waspProjectCommandBuilders)
      ~&& unwords ["cd", fromAbsDir testContext.testCaseDir]

createTestWaspProject :: WaspNewTemplate -> ShellCommandBuilder TestContext ShellCommand
createTestWaspProject template = do
  testContext <- ask
  waspCliNew (_waspProjectName testContext.waspProjectContext) template
