module Wasp.Generator.WaspLibs.WaspLib
  ( WaspLib (..),
    makeWaspLib,
    makeLocalNpmDepFromWaspLib,
    getTarballPathInLibsRootDir,
    getTarballAbsPathInLibsSourceDir,
  )
where

import StrongPath (Abs, Dir, File', Path', Rel, Rel', fromRelFile, (</>))
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import Wasp.ExternalConfig.Npm.Tarball (TarballFilename, tarballFilenameAsRelFile)
import qualified Wasp.ExternalConfig.Npm.Tarball as Npm.Tarball
import Wasp.Generator.WaspLibs.Common (LibsRootDir, LibsSourceDir)
import qualified Wasp.Version

{-
  `WaspLib` represents an internal Wasp npm package that are located in the
  ./libs directory. This npm package contain code that is used in the generated
  Wasp app. They are packaged into npm tarballs which are copied to the
  generated Wasp app and are installed as an npm dependency.
-}
data WaspLib = WaspLib
  { packageName :: String,
    tarballFilename :: TarballFilename
  }

makeWaspLib :: String -> WaspLib
makeWaspLib waspLibPackageName =
  WaspLib
    { packageName = waspLibPackageName,
      tarballFilename = Npm.Tarball.makeTarballFilename waspLibPackageName waspVersionStr
    }
  where
    waspVersionStr = show Wasp.Version.waspVersion

makeLocalNpmDepFromWaspLib :: Path' Rel' (Dir LibsRootDir) -> WaspLib -> Npm.Dependency.Dependency
makeLocalNpmDepFromWaspLib tarballSrcDir waspLib = Npm.Dependency.make (packageName waspLib, npmDepFilePath)
  where
    npmDepFilePath = "file:" <> fromRelFile (tarballSrcDir </> getTarballPathInLibsRootDir waspLib)

-- | Gets the relative path to a WaspLib tarball within LibsRootDir.
-- Tarballs are stored at the top level of LibsRootDir (flat structure, no subdirectories).
getTarballPathInLibsRootDir :: WaspLib -> Path' (Rel LibsRootDir) File'
getTarballPathInLibsRootDir = tarballFilenameAsRelFile . tarballFilename

getTarballAbsPathInLibsSourceDir :: Path' Abs (Dir LibsSourceDir) -> WaspLib -> Path' Abs File'
getTarballAbsPathInLibsSourceDir libsSourceDir =
  (libsSourceDir </>)
    . tarballFilenameAsRelFile
    . tarballFilename
