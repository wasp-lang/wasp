module Wasp.PackageManager.Detection
  ( Detection (..),
    DetectionSource (..),
    detectProjectPackageManager,
    detectInvokingPackageManager,
  )
where

import Control.Monad.Extra (fromMaybeM)
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import System.Environment (lookupEnv)
import Wasp.PackageManager.Detection.Common (Detection (..), DetectionSource (..))
import Wasp.PackageManager.Detection.ProjectFiles (detectFromProjectFiles)
import Wasp.PackageManager.PackageManager (defaultPackageManager)
import Wasp.PackageManager.UserAgent (parseUserAgent, userAgentEnvVarName)

-- | The package manager of the project in the given directory or of the
-- workspace it belongs to, falling back to the package manager that launched
-- Wasp.
detectProjectPackageManager :: Path' Abs (Dir d) -> IO Detection
detectProjectPackageManager dir =
  fromMaybeM detectInvokingPackageManager $ detectFromProjectFiles $ SP.castDir dir

-- | The package manager that launched Wasp, falling back to
-- 'defaultPackageManager'.
detectInvokingPackageManager :: IO Detection
detectInvokingPackageManager =
  maybe (Detection defaultPackageManager Default) (`Detection` UserAgent) . (>>= parseUserAgent)
    <$> lookupEnv userAgentEnvVarName
