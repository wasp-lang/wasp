module Wasp.PackageManager.PackageManager
  ( PackageManager (..),
    defaultPackageManager,
    supportedPackageManagers,
    packageManagerName,
    parsePackageManagerName,
  )
where

import Data.List (find)

data PackageManager = Npm | Pnpm | Yarn | Bun
  deriving (Eq, Show, Enum, Bounded)

-- | Used when nothing tells us which package manager the user wants, or when
-- they want one that isn't in 'supportedPackageManagers'.
defaultPackageManager :: PackageManager
defaultPackageManager = Npm

-- | The package managers Wasp can run a project with. The others are still
-- detected, so that we can tell the user we don't support theirs.
supportedPackageManagers :: [PackageManager]
supportedPackageManagers = [Npm]

{- HLINT ignore packageManagerName "Avoid npm and npx" -}

-- | The name the package manager goes by, which is also its executable.
packageManagerName :: PackageManager -> String
packageManagerName = \case
  Npm -> "npm"
  Pnpm -> "pnpm"
  Yarn -> "yarn"
  Bun -> "bun"

parsePackageManagerName :: String -> Maybe PackageManager
parsePackageManagerName name = find ((== name) . packageManagerName) [minBound .. maxBound]
