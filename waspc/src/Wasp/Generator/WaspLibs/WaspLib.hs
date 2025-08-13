module Wasp.Generator.WaspLibs.WaspLib
  ( WaspLib (..),
    makeWaspLib,
    makeNpmDependencyForWaspLib,
  )
where

import StrongPath (Dir, File', Path', Rel, Rel', castRel, fromRelFile, (</>))
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import qualified Wasp.ExternalConfig.Npm.Tarball as Npm.Tarball
import Wasp.Generator.WaspLibs.Common (LibsRootDir, LibsSourceDir, getAbsLibsSourceDirPath)
import qualified Wasp.SemanticVersion as SV
import Wasp.Util (checksumFromFilePath, hexToString)

data WaspLib = WaspLib
  { name :: String,
    srcTarballPath :: Path' (Rel LibsSourceDir) File',
    dstTarballPath :: Path' (Rel LibsRootDir) File'
  }

makeWaspLib :: String -> IO WaspLib
makeWaspLib waspLibName = do
  tarballChecksum <- computeTarballChecksum tarballPath

  return $
    WaspLib
      { name = waspLibName,
        srcTarballPath = tarballPath,
        dstTarballPath = Npm.Tarball.makeTarballFilePath sanitizedTarballName tarballChecksum
      }
  where
    tarballPath = Npm.Tarball.makeTarballFilePath sanitizedTarballName waspLibVersion
    sanitizedTarballName = Npm.Tarball.sanitizeForTarballFilename waspLibName
    -- Use don't version the libs, so we use a dummy version here and in the
    -- libs source directory.
    waspLibVersion = show $ SV.Version 0 0 0

computeTarballChecksum :: Path' (Rel dir) File' -> IO String
computeTarballChecksum tarballPath = do
  tarballSrcPath <- (</> castRel tarballPath) <$> getAbsLibsSourceDirPath
  take 8 . hexToString <$> checksumFromFilePath tarballSrcPath

makeNpmDependencyForWaspLib :: Path' Rel' (Dir LibsRootDir) -> WaspLib -> Npm.Dependency.Dependency
makeNpmDependencyForWaspLib tarballRelDir waspLib =
  Npm.Dependency.make
    (name waspLib, npmDepFilePath)
  where
    npmDepFilePath = "file:" <> fromRelFile (tarballRelDir </> dstTarballPath waspLib)
