module Wasp.PackageManager.PackageManager
  ( PackageManager (..),
    defaultPackageManager,
    packageManagerName,
    parsePackageManagerName,
  )
where

import Data.List (find)

data PackageManager = Npm | Pnpm | Bun
  deriving (Eq, Show, Enum, Bounded)

-- | Used when nothing tells us which package manager the user wants.
defaultPackageManager :: PackageManager
defaultPackageManager = Npm

{- HLINT ignore packageManagerName "Avoid npm and npx" -}

-- | The name the package manager goes by, which is also its executable.
packageManagerName :: PackageManager -> String
packageManagerName = \case
  Npm -> "npm"
  Pnpm -> "pnpm"
  Bun -> "bun"

parsePackageManagerName :: String -> Maybe PackageManager
parsePackageManagerName name = find ((== name) . packageManagerName) [minBound .. maxBound]
