module Wasp.Generator.WaspLibs.WaspLib
  ( WaspLib (..),
    makeWaspLib,
    makeLocalNpmDepFromWaspLib,
    getTarballPathInLibsRootDir,
  )
where

import StrongPath (Abs, Dir, File', Path', Rel, Rel', fromRelFile, (</>))
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import Wasp.ExternalConfig.Npm.Tarball (TarballFilename, tarballFilenameAsRelFile)
import qualified Wasp.ExternalConfig.Npm.Tarball as Npm.Tarball
import Wasp.Generator.WaspLibs.Common (LibsRootDir, getAbsLibsSourceDirPath)
import qualified Wasp.Version

{-
  `WaspLib` represents an internal Wasp npm package that are located in the
  ./libs directory. This npm package contain code that is used in the generated
  Wasp app. They are packaged into npm tarballs which are copied to the
  generated Wasp app and are installed as an npm dependency.
-}
data WaspLib = WaspLib
  { packageName :: String,
    waspDataDirTarballAbsPath :: Path' Abs File',
    tarballFilename :: TarballFilename
  }

makeWaspLib :: String -> IO WaspLib
makeWaspLib waspLibPackageName = do
  let tarballFilename' = Npm.Tarball.makeTarballFilename waspLibPackageName waspVersionStr
  waspDataDirTarballAbsPath' <- (</> tarballFilenameAsRelFile tarballFilename') <$> getAbsLibsSourceDirPath

  return $
    WaspLib
      { packageName = waspLibPackageName,
        waspDataDirTarballAbsPath = waspDataDirTarballAbsPath',
        tarballFilename = tarballFilename'
      }
  where
    waspVersionStr = show $ Wasp.Version.waspVersion

makeLocalNpmDepFromWaspLib :: Path' Rel' (Dir LibsRootDir) -> WaspLib -> Npm.Dependency.Dependency
makeLocalNpmDepFromWaspLib tarballSrcDir waspLib = Npm.Dependency.make (packageName waspLib, npmDepFilePath)
  where
    npmDepFilePath = "file:" <> fromRelFile (tarballSrcDir </> getTarballPathInLibsRootDir waspLib)

-- | Gets the relative path to a WaspLib tarball within LibsRootDir.
-- Tarballs are stored at the top level of LibsRootDir (flat structure, no subdirectories).
getTarballPathInLibsRootDir :: WaspLib -> Path' (Rel LibsRootDir) File'
getTarballPathInLibsRootDir = tarballFilenameAsRelFile . tarballFilename
