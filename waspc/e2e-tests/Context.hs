{-# LANGUAGE DuplicateRecordFields #-}

module Context
  ( WaspProjectContext (..),
    TestContext (..),
    SnapshotTestContext (..),
    HasWorkingDir (..),
  )
where

import FileSystem (SnapshotDir, TestCaseDir)
import StrongPath (Abs, Dir, Dir', Path', castDir)
import Wasp.Project (WaspProjectDir)

-- | Context for steps which are run from inside of a Wasp app project.
data WaspProjectContext = WaspProjectContext
  { waspProjectDir :: Path' Abs (Dir WaspProjectDir),
    waspProjectName :: String
  }

-- | Context for steps of a 'Test.Test', run from the 'FileSystem.TestCaseDir' directory.
data TestContext = TestContext
  { testCaseDir :: Path' Abs (Dir TestCaseDir),
    waspProjectContext :: WaspProjectContext
  }

-- | Context for steps of a 'SnapshotTest.SnapshotTest', run from the 'FileSystem.SnapshotDir' directory.
data SnapshotTestContext = SnapshotTestContext
  { snapshotDir :: Path' Abs (Dir SnapshotDir),
    waspProjectContext :: WaspProjectContext
  }

-- | Every step context designates the directory that commands run in and that
-- relative file paths resolve against.
class HasWorkingDir ctx where
  workingDir :: ctx -> Path' Abs Dir'

instance HasWorkingDir WaspProjectContext where
  workingDir = castDir . (.waspProjectDir)

instance HasWorkingDir TestContext where
  workingDir = castDir . (.testCaseDir)

instance HasWorkingDir SnapshotTestContext where
  workingDir = castDir . (.snapshotDir)
