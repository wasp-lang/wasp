module Wasp.Generator.WaspLibs.WaspLib
  ( WaspLib (..),
    makeWaspLib,
    makeNpmDependencyForWaspLib,
  )
where

import StrongPath (Abs, Dir, File', Path', Rel', fromRelFile, (</>))
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import Wasp.ExternalConfig.Npm.Tarball (NpmTarball)
import qualified Wasp.ExternalConfig.Npm.Tarball as Npm.Tarball
import Wasp.Generator.WaspLibs.Common (LibsRootDir, LibsSourceDir, getAbsLibsSourceDirPath)
import qualified Wasp.SemanticVersion as SV
import Wasp.Util (checksumFromFilePath, hexToString)

data WaspLib = WaspLib
  { packageName :: String,
    dataDirTarball :: NpmTarball LibsSourceDir,
    generatedCodeDirTarball :: NpmTarball LibsRootDir
  }

makeWaspLib :: String -> IO WaspLib
makeWaspLib waspLibPackageName = do
  tarballChecksum <- computeTarballChecksum . (</> Npm.Tarball.filename tarball) =<< getAbsLibsSourceDirPath

  return $
    WaspLib
      { packageName = waspLibPackageName,
        dataDirTarball = tarball,
        generatedCodeDirTarball = Npm.Tarball.makeNpmTarball sanitizedTarballName tarballChecksum
      }
  where
    tarball = Npm.Tarball.makeNpmTarball sanitizedTarballName waspLibVersion
    sanitizedTarballName = Npm.Tarball.sanitizePackageNameForTarballName waspLibPackageName
    -- Use don't version the libs, so we use a dummy version here and in the
    -- libs source directory.
    waspLibVersion = show $ SV.Version 0 0 0

computeTarballChecksum :: Path' Abs File' -> IO String
computeTarballChecksum tarballPath = take 8 . hexToString <$> checksumFromFilePath tarballPath

makeNpmDependencyForWaspLib :: Path' Rel' (Dir LibsRootDir) -> WaspLib -> Npm.Dependency.Dependency
makeNpmDependencyForWaspLib tarballRelDir waspLib = Npm.Dependency.make (packageName waspLib, npmDepFilePath)
  where
    npmDepFilePath = "file:" <> fromRelFile (tarballRelDir </> (Npm.Tarball.filename . generatedCodeDirTarball $ waspLib))
