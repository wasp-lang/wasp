module Wasp.Generator.WaspLibs.WaspLib
  ( WaspLib (..),
    makeWaspLib,
    makeLocalNpmDepFromWaspLib,
    getLibDirPathInLibsRootDir,
  )
where

import Data.Maybe (fromJust)
import StrongPath (Dir', Path', Rel, Rel', fromRelDir, (</>))
import qualified StrongPath as SP
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import Wasp.Generator.WaspLibs.Common (LibsRootDir)

{-
  `WaspLib` represents an internal Wasp npm package that are located in the
  ./libs directory. This npm package contain code that is used in the generated
  Wasp app. They are copied to the generated Wasp app and are installed as an
  npm dependency via file paths.
-}
data WaspLib = WaspLib
  { packageName :: String,
    libDirName :: String
  }
  deriving (Show, Eq)

-- | Creates a WaspLib from a package name.
-- Pure function - no IO needed since we're no longer computing checksums.
makeWaspLib :: String -> WaspLib
makeWaspLib waspLibPackageName =
  WaspLib
    { packageName = waspLibPackageName,
      libDirName = getLibDirNameFromPackageName waspLibPackageName
    }

-- | Extracts the library directory name from the package name.
-- For example, "@wasp.sh/lib-auth" becomes "auth".
getLibDirNameFromPackageName :: String -> String
getLibDirNameFromPackageName pkgName =
  case reverse $ takeWhile (/= '-') $ reverse pkgName of
    [] -> pkgName
    dirName -> dirName

makeLocalNpmDepFromWaspLib :: Path' Rel' (SP.Dir LibsRootDir) -> WaspLib -> Npm.Dependency.Dependency
makeLocalNpmDepFromWaspLib libsSrcDir waspLib = Npm.Dependency.make (packageName waspLib, npmDepFilePath)
  where
    npmDepFilePath = "file:" <> fromRelDir (libsSrcDir </> getLibDirPathInLibsRootDir waspLib)

-- | Gets the relative path to a WaspLib directory within LibsRootDir.
-- Libraries are stored in subdirectories named after the lib (e.g., libs/auth/).
getLibDirPathInLibsRootDir :: WaspLib -> Path' (Rel LibsRootDir) Dir'
getLibDirPathInLibsRootDir waspLib = fromJust $ SP.parseRelDir (libDirName waspLib)
