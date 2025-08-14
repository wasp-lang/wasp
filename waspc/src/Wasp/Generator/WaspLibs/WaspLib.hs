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
