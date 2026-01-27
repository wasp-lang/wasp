module Wasp.Generator.WaspLibs.WaspLib
  ( WaspLib (..),
    makeWaspLib,
    makeLocalNpmDepFromWaspLib,
    getTarballPathInLibsRootDir,
    getTarballPathInLibsSourceDir,
  )
where

import StrongPath (Dir, Dir', File', Path', Rel, Rel', fromRelFile, (</>))
import qualified StrongPath as SP
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import Wasp.ExternalConfig.Npm.Tarball (TarballFilename, tarballFilenameAsRelFile)
import qualified Wasp.ExternalConfig.Npm.Tarball as Npm.Tarball
import Wasp.Generator.WaspLibs.Common (LibsRootDir, LibsSourceDir)
import Wasp.Version (waspVersion)

{-
  `WaspLib` represents an internal Wasp npm package that are located in the
  ./data/Generator/libs directory. This npm package contain code that is used in the generated
  Wasp app. They are packaged into npm tarballs which are copied to the
  generated Wasp app and are installed as an npm dependency.
-}
data WaspLib = WaspLib
  { packageName :: String,
    libDirName :: Path' Rel' Dir',
    tarballFilename :: TarballFilename
  }

makeWaspLib :: String -> Path' Rel' Dir' -> WaspLib
makeWaspLib waspLibPackageName libDirName' =
  WaspLib
    { packageName = waspLibPackageName,
      libDirName = libDirName',
      tarballFilename = Npm.Tarball.makeTarballFilename waspLibPackageName waspVersionStr
    }
  where
    waspVersionStr = show waspVersion

makeLocalNpmDepFromWaspLib :: Path' Rel' (Dir LibsRootDir) -> WaspLib -> Npm.Dependency.Dependency
makeLocalNpmDepFromWaspLib tarballSrcDir waspLib = Npm.Dependency.make (packageName waspLib, npmDepFilePath)
  where
    npmDepFilePath = "file:" <> fromRelFile (tarballSrcDir </> getTarballPathInLibsRootDir waspLib)

-- | Tarballs are shipped with the CLI in subdirectories in the LibsSourceDir.
getTarballPathInLibsSourceDir :: WaspLib -> Path' (Rel LibsSourceDir) File'
getTarballPathInLibsSourceDir = getTarballPath

-- | Tarballs are copied to subdirectories in the LibsRootDir in the generated Wasp app.
getTarballPathInLibsRootDir :: WaspLib -> Path' (Rel LibsRootDir) File'
getTarballPathInLibsRootDir = getTarballPath

getTarballPath :: WaspLib -> Path' (Rel rel) File'
getTarballPath waspLib = SP.castRel (libDirName waspLib) </> (tarballFilenameAsRelFile . tarballFilename $ waspLib)
