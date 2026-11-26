{-- This module describes the `PackageManager` public API --}
module Wasp.PackageManager
  ( module Wasp.PackageManager.PackageManager,
    module Wasp.PackageManager.Detection,
  )
where

import Wasp.PackageManager.Detection
  ( Detection (..),
    DetectionSource (..),
    detectInvokingPackageManager,
    detectProjectPackageManager,
  )
import Wasp.PackageManager.PackageManager
  ( PackageManager (..),
    packageManagerName,
    parsePackageManagerName,
  )
