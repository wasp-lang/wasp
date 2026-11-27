module Wasp.PackageManager.Detection
  ( DetectionWarning,
    detectProjectPackageManager,
    detectInvokingPackageManager,
  )
where

import Control.Monad.Extra (firstJustM)
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import Wasp.PackageManager.Detection.Common (Detection (..), DetectionSource (..))
import Wasp.PackageManager.Detection.ProjectFiles (detectFromProjectFiles)
import Wasp.PackageManager.Detection.UserAgent (detectFromUserAgent)
import Wasp.PackageManager.PackageManager (PackageManager, defaultPackageManager, packageManagerName, supportedPackageManagers)

type DetectionWarning = String

-- | Gives the package manager Wasp should use in an existing project.
--
-- Detects it from the files of the project in the given directory, then from
-- its workspace root, then from the package manager that launched Wasp. If
-- none of them say anything, gives 'defaultPackageManager'.
--
-- If Wasp doesn't support the detected package manager, gives
-- 'defaultPackageManager' and a warning to show to the user.
detectProjectPackageManager :: Path' Abs (Dir d) -> IO (PackageManager, Maybe DetectionWarning)
detectProjectPackageManager dir =
  choosePackageManagerToUse <$> firstJustM id [detectFromProjectFiles $ SP.castDir dir, detectFromUserAgent]

-- | Gives the package manager Wasp should use when there is no project yet,
-- for example when creating one.
--
-- Detects it from the package manager that launched Wasp. If that says
-- nothing, gives 'defaultPackageManager'.
--
-- If Wasp doesn't support the detected package manager, gives
-- 'defaultPackageManager' and a warning to show to the user.
detectInvokingPackageManager :: IO (PackageManager, Maybe DetectionWarning)
detectInvokingPackageManager = choosePackageManagerToUse <$> detectFromUserAgent

choosePackageManagerToUse :: Maybe Detection -> (PackageManager, Maybe DetectionWarning)
choosePackageManagerToUse = \case
  Nothing -> (defaultPackageManager, Nothing)
  Just detection
    | detection.packageManager `elem` supportedPackageManagers -> (detection.packageManager, Nothing)
    | otherwise -> (defaultPackageManager, Just $ unsupportedWarning detection)
  where
    unsupportedWarning detection =
      "Wasp detected "
        ++ packageManagerName detection.packageManager
        ++ " from "
        ++ describeSource detection.source
        ++ ", but doesn't support it yet. Wasp will use "
        ++ packageManagerName defaultPackageManager
        ++ " instead."

    describeSource = \case
      PackageManagerField file -> "the packageManager field in " ++ SP.fromAbsFile file
      DevEnginesField file -> "the devEngines field in " ++ SP.fromAbsFile file
      LockfileOrWorkspaceFile file -> SP.fromAbsFile file
      InstallMetadata file -> SP.fromAbsFile file
      UserAgent -> "the command that started it"
