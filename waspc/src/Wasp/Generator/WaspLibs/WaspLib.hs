module Wasp.Generator.WaspLibs.WaspLib
  ( WaspLib (..),
    makeWaspLib,
    makeNpmDependencyForWaspLib,
  )
where

import StrongPath (Dir', File', Path', Rel', fromRelFile, (</>))
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import qualified Wasp.ExternalConfig.Npm.Tarball as Npm.Tarball
import qualified Wasp.Version as WV

data WaspLib = WaspLib
  { name :: String,
    tarballRelFile :: Path' Rel' File'
  }

makeWaspLib :: String -> WaspLib
makeWaspLib waspLibName =
  WaspLib
    { name = waspLibName,
      tarballRelFile = Npm.Tarball.makeTarballFilePath waspLibName waspLibVersion
    }
  where
    -- We expect the libs to have the same version as the Wasp CLI
    waspLibVersion = show WV.waspVersion

makeNpmDependencyForWaspLib :: Path' Rel' Dir' -> WaspLib -> Npm.Dependency.Dependency
makeNpmDependencyForWaspLib tarballRelDir waspLib =
  Npm.Dependency.make
    (name waspLib, npmDepAbsFilePath)
  where
    npmDepAbsFilePath = "file:" <> fromRelFile (tarballRelDir </> tarballRelFile waspLib)
