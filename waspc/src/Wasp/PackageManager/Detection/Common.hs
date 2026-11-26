module Wasp.PackageManager.Detection.Common
  ( Detection (..),
    DetectionSource (..),
  )
where

import StrongPath (Abs, File', Path')
import Wasp.PackageManager.PackageManager (PackageManager)

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
  | -- | Nothing told us, so this is 'defaultPackageManager'.
    Default
  deriving (Eq, Show)
