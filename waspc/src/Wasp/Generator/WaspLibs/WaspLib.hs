module Wasp.Generator.WaspLibs.WaspLib
  ( WaspLib (..),
    makeWaspLib,
    makeLocalNpmDepFromWaspLib,
  )
where

import StrongPath (Abs, Dir, File', Path', Rel', fromRelFile, (</>))
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import Wasp.ExternalConfig.Npm.Tarball (TarballFilename, tarballFilenameAsRelFile)
import qualified Wasp.ExternalConfig.Npm.Tarball as Npm.Tarball
import Wasp.Generator.WaspLibs.Common (LibsRootDir, getAbsLibsSourceDirPath)
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
    waspDataDirTarballFilename :: TarballFilename,
    waspDataDirTarballAbsPath :: Path' Abs File',
    generatedCodeDirTarballFilename :: TarballFilename
  }

makeWaspLib :: String -> IO WaspLib
makeWaspLib waspLibPackageName = do
  libsSourceDirPath <- getAbsLibsSourceDirPath

  -- Libs have a fixed version "0.0.0" which means we use the same version for the tarballs
  -- in the data directory (e.g. lib-0.0.0.tgz). When the tarballs are copied to the generated project directory,
  -- the tarball filename version is replaced with the checksum of the tarball (e.g. lib-<checksum>.tgz).
  let waspDataDirTarballFilename' = Npm.Tarball.makeTarballFilename waspLibPackageName "0.0.0"
  let waspDataDirTarballAbsPath' = libsSourceDirPath </> tarballFilenameAsRelFile waspDataDirTarballFilename'

  waspDataDirTarballChecksum <- computeTarballChecksum waspDataDirTarballAbsPath'
  let generatedCodeDirTarballilename' = Npm.Tarball.makeTarballFilename waspLibPackageName waspDataDirTarballChecksum

  return $
    WaspLib
      { packageName = waspLibPackageName,
        waspDataDirTarballFilename = waspDataDirTarballFilename',
        waspDataDirTarballAbsPath = waspDataDirTarballAbsPath',
        generatedCodeDirTarballFilename = generatedCodeDirTarballilename'
      }

computeTarballChecksum :: Path' Abs File' -> IO String
computeTarballChecksum tarballPath = take 8 . hexToString <$> checksumFromFilePath tarballPath

makeLocalNpmDepFromWaspLib :: Path' Rel' (Dir LibsRootDir) -> WaspLib -> Npm.Dependency.Dependency
makeLocalNpmDepFromWaspLib tarbalSrcDir waspLib = Npm.Dependency.make (packageName waspLib, npmDepFilePath)
  where
    npmDepFilePath = "file:" <> fromRelFile (tarbalSrcDir </> tarballFilenameAsRelFile (generatedCodeDirTarballFilename waspLib))
