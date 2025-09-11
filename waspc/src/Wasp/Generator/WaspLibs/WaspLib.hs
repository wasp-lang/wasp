module Wasp.Generator.WaspLibs.WaspLib
  ( WaspLib (..),
    makeWaspLib,
    waspLibAsNpmDependency,
  )
where

import StrongPath (Abs, Dir, File', Path', Rel', fromRelFile, (</>))
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import Wasp.ExternalConfig.Npm.Tarball (NpmTarball)
import qualified Wasp.ExternalConfig.Npm.Tarball as Npm.Tarball
import Wasp.Generator.WaspLibs.Common (LibsRootDir, LibsSourceDir, getAbsLibsSourceDirPath)
import Wasp.Util (checksumFromFilePath, hexToString)

{-
  `WaspLib` represents internal Wasp npm packages that are located in the
  ./libs directory. These packages contain code that is used in the generated
  Wasp app. `WaspLib`s are packaged into npm tarballs which are copied to the
  generated Wasp app and are installed as npm dependencies.

  The filename of a npm tarball copied to the generated Wasp app contains the checksum of the
  tarball, to avoid npm caching the tarball.
-}
data WaspLib = WaspLib
  { packageName :: String,
    waspDataDirTarball :: NpmTarball LibsSourceDir,
    waspDataDirTarballAbsPath :: Path' Abs File',
    generatedCodeDirTarball :: NpmTarball LibsRootDir
  }

makeWaspLib :: String -> IO WaspLib
makeWaspLib waspLibPackageName = do
  libsSourceDirPath <- getAbsLibsSourceDirPath

  -- Libs have a fixed version "0.0.0" which means we use the same version for the tarballs
  -- in the data directory (e.g. lib-0.0.0.tgz). When the tarballs are copied to the generated project directory,
  -- the tarball filename version is replaced with the checksum of the tarball (e.g. lib-<checksum>.tgz).
  let waspDataDirTarball' = Npm.Tarball.makeNpmTarball sanitizedTarballName "0.0.0"
  let waspDataDirTarballAbsPath' = libsSourceDirPath </> Npm.Tarball.filename waspDataDirTarball'

  waspDataDirTarballChecksum <- computeTarballChecksum waspDataDirTarballAbsPath'
  let generatedCodeDirTarball' = Npm.Tarball.makeNpmTarball sanitizedTarballName waspDataDirTarballChecksum

  return $
    WaspLib
      { packageName = waspLibPackageName,
        waspDataDirTarball = waspDataDirTarball',
        waspDataDirTarballAbsPath = waspDataDirTarballAbsPath',
        generatedCodeDirTarball = generatedCodeDirTarball'
      }
  where
    sanitizedTarballName = Npm.Tarball.sanitizePackageNameForTarballName waspLibPackageName

computeTarballChecksum :: Path' Abs File' -> IO String
computeTarballChecksum tarballPath = take 8 . hexToString <$> checksumFromFilePath tarballPath

waspLibAsNpmDependency :: Path' Rel' (Dir LibsRootDir) -> WaspLib -> Npm.Dependency.Dependency
waspLibAsNpmDependency tarballRelDir waspLib = Npm.Dependency.make (packageName waspLib, npmDepFilePath)
  where
    npmDepFilePath = "file:" <> fromRelFile (tarballRelDir </> (Npm.Tarball.filename . generatedCodeDirTarball $ waspLib))
