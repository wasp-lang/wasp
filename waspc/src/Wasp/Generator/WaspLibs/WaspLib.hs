module Wasp.Generator.WaspLibs.WaspLib
  ( WaspLib (..),
    makeWaspLib,
    makeLocalNpmDepFromWaspLib,
    getTarballPathInLibsRootDir,
  )
where

import StrongPath (Abs, Dir, File', Path', Rel, Rel', fromRelFile, (</>))
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import Wasp.ExternalConfig.Npm.Tarball (TarballFilename (..), tarballFilenameAsRelFile)
import Wasp.Generator.WaspLibs.Common (LibsRootDir, getAbsLibsSourceDirPath)
import qualified Wasp.Generator.WaspLibs.Manifest as Manifest

{-
  `WaspLib` represents an internal Wasp npm package that are located in the
  ./libs directory. This npm package contain code that is used in the generated
  Wasp app. They are packaged into npm tarballs which are copied to the
  generated Wasp app and are installed as an npm dependency.

  The tarball filename is loaded from manifest.json at compile time, which maps
  package names to their tarball filenames.

  Wasp compiler needs the manifest file to know the tarball filename because:
  - while developing Wasp -> tarballs have a random version for cache busting purposes,
  - when shipped to users -> tarballs have a static version that equals the Wasp version.
-}
data WaspLib = WaspLib
  { packageName :: String,
    waspDataDirTarballAbsPath :: Path' Abs File',
    generatedCodeDirTarballFilename :: TarballFilename
  }

makeWaspLib :: String -> IO WaspLib
makeWaspLib waspLibPackageName = do
  tarballFilename <- case Manifest.getTarballFilename waspLibPackageName of
    Just filename -> return $ TarballFilename filename
    Nothing -> error $ "No tarball found in manifest for package: " ++ waspLibPackageName

  waspDataDirTarballAbsPath' <- (</> tarballFilenameAsRelFile tarballFilename) <$> getAbsLibsSourceDirPath

  return $
    WaspLib
      { packageName = waspLibPackageName,
        waspDataDirTarballAbsPath = waspDataDirTarballAbsPath',
        generatedCodeDirTarballFilename = tarballFilename
      }

makeLocalNpmDepFromWaspLib :: Path' Rel' (Dir LibsRootDir) -> WaspLib -> Npm.Dependency.Dependency
makeLocalNpmDepFromWaspLib tarballSrcDir waspLib = Npm.Dependency.make (packageName waspLib, npmDepFilePath)
  where
    npmDepFilePath = "file:" <> fromRelFile (tarballSrcDir </> getTarballPathInLibsRootDir waspLib)

-- | Gets the relative path to a WaspLib tarball within LibsRootDir.
-- Tarballs are stored at the top level of LibsRootDir (flat structure, no subdirectories).
getTarballPathInLibsRootDir :: WaspLib -> Path' (Rel LibsRootDir) File'
getTarballPathInLibsRootDir = tarballFilenameAsRelFile . generatedCodeDirTarballFilename
