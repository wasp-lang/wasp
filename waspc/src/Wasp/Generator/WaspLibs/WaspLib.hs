module Wasp.Generator.WaspLibs.WaspLib
  ( WaspLib (..),
    makeWaspLib,
    makeNpmDependencyForWaspLib,
  )
where

import StrongPath (Dir', File', Path', Rel', fromRelFile, (</>))
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import Wasp.ExternalConfig.Npm.Tarball (SanitizedTarballName)
import qualified Wasp.ExternalConfig.Npm.Tarball as Npm.Tarball
import qualified Wasp.SemanticVersion as SV

data WaspLib = WaspLib
  { name :: String,
    tarballName :: SanitizedTarballName,
    srcTarballPath :: Path' Rel' File',
    dstTarballPath :: Path' Rel' File'
  }

makeWaspLib :: String -> WaspLib
makeWaspLib waspLibName =
  WaspLib
    { name = waspLibName,
      tarballName = sanitizedTarballName,
      srcTarballPath = tarballPath,
      dstTarballPath = tarballPath
    }
  where
    tarballPath = Npm.Tarball.makeTarballFilePath sanitizedTarballName waspLibVersion
    sanitizedTarballName = Npm.Tarball.sanitizeForTarballFilename waspLibName

-- Hard coded version becuase the tarballPath will have the contents
-- checksum appended later
waspLibVersion :: String
waspLibVersion = show $ SV.Version 0 0 0

makeNpmDependencyForWaspLib :: Path' Rel' Dir' -> WaspLib -> Npm.Dependency.Dependency
makeNpmDependencyForWaspLib tarballRelDir waspLib =
  Npm.Dependency.make
    (name waspLib, npmDepAbsFilePath)
  where
    npmDepAbsFilePath = "file:" <> fromRelFile (tarballRelDir </> dstTarballPath waspLib)
