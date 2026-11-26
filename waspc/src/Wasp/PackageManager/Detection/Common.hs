module Wasp.PackageManager.Detection.Common
  ( Detection (..),
    DetectionSource (..),
    PackageRootDir,
  )
where

import StrongPath (Abs, File', Path')
import Wasp.PackageManager.PackageManager (PackageManager)

-- | The root directory of a package, where its package.json lives. That is
-- the directory detection starts in, or, when it starts in a package inside a
-- monorepo, the parent directory that is its workspace root.
data PackageRootDir

data Detection = Detection
  { packageManager :: PackageManager,
    source :: DetectionSource
  }
  deriving (Eq, Show)

-- | What told us the package manager, with the file when there is one.
data DetectionSource
  = PackageManagerField (Path' Abs File')
  | DevEnginesField (Path' Abs File')
  | LockfileOrWorkspaceFile (Path' Abs File')
  | InstallMetadata (Path' Abs File')
  | UserAgent
  deriving (Eq, Show)
