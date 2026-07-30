{-# LANGUAGE DuplicateRecordFields #-}

module Context
  ( WaspProjectContext (..),
    makeWaspProjectContext,
    TestContext (..),
    SnapshotTestContext (..),
    HasWorkingDir (..),
    HasWaspProjectContext (..),
  )
where

import Data.Maybe (fromJust)
import FileSystem (SnapshotDir, TestCaseDir)
import StrongPath (Abs, Dir, Dir', Path', castDir, parseRelDir, (</>))
import Wasp.Project (WaspProjectDir)

-- | Context for steps which are run from inside of a Wasp app project.
data WaspProjectContext = WaspProjectContext
  { waspProjectDir :: Path' Abs (Dir WaspProjectDir),
    waspProjectName :: String
  }

-- | The context for the conventionally-named Wasp project that every test
-- creates directly inside its own directory (test case dir or snapshot dir).
makeWaspProjectContext :: Path' Abs (Dir d) -> WaspProjectContext
makeWaspProjectContext parentDir =
  WaspProjectContext
    { waspProjectDir = parentDir </> (fromJust . parseRelDir $ waspProjectName),
      waspProjectName
    }
  where
    waspProjectName = "wasp-app"

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

-- | A step context that wraps a 'WaspProjectContext', so that Wasp project
-- steps can run from either a 'TestContext' or a 'SnapshotTestContext'.
class HasWaspProjectContext ctx where
  getWaspProjectContext :: ctx -> WaspProjectContext

instance HasWaspProjectContext TestContext where
  getWaspProjectContext = (.waspProjectContext)

instance HasWaspProjectContext SnapshotTestContext where
  getWaspProjectContext = (.waspProjectContext)
