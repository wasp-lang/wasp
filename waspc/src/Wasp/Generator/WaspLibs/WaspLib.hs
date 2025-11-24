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
import Wasp.Util (checksumFromFilePath, hexToString)

{-
  `WaspLib` represents an internal Wasp npm package that are located in the
  ./libs directory. This npm package contain code that is used in the generated
  Wasp app. They are packaged into npm tarballs which are copied to the
  generated Wasp app and are installed as an npm dependency.

  The filename of a npm tarball copied to the generated Wasp app contains the checksum of the
  tarball, to avoid npm caching the tarball.
-}
data WaspLib = WaspLib
  { packageName :: String,
    waspDataDirTarballAbsPath :: Path' Abs File',
    generatedCodeDirTarballFilename :: TarballFilename
  }

makeWaspLib :: String -> IO WaspLib
makeWaspLib waspLibPackageName = do
  -- Wasp lib tarballs have a fixed version 0.0.0 (lib-auth-0.0.0.tgz) when shipped with Wasp.
  -- When the libs are copied to a generated project, the 0.0.0 is replaced with the lib's
  -- checksum to avoid npm cache issues. See `waspc/libs/README.md` for detailed explanation.
  waspDataDirTarballAbsPath' <- (</> tarballFilenameAsRelFile (Npm.Tarball.makeTarballFilename waspLibPackageName "0.0.0")) <$> getAbsLibsSourceDirPath
  generatedCodeDirTarballFilename' <- Npm.Tarball.makeTarballFilename waspLibPackageName <$> computeTarballChecksum waspDataDirTarballAbsPath'

  return $
    WaspLib
      { packageName = waspLibPackageName,
        waspDataDirTarballAbsPath = waspDataDirTarballAbsPath',
        generatedCodeDirTarballFilename = generatedCodeDirTarballFilename'
      }

computeTarballChecksum :: Path' Abs File' -> IO String
computeTarballChecksum tarballPath = take 8 . hexToString <$> checksumFromFilePath tarballPath

makeLocalNpmDepFromWaspLib :: Path' Rel' (Dir LibsRootDir) -> WaspLib -> Npm.Dependency.Dependency
makeLocalNpmDepFromWaspLib tarballSrcDir waspLib = Npm.Dependency.make (packageName waspLib, npmDepFilePath)
  where
    npmDepFilePath = "file:" <> fromRelFile (tarballSrcDir </> getTarballPathInLibsRootDir waspLib)

-- | Gets the relative path to a WaspLib tarball within LibsRootDir.
-- Tarballs are stored at the top level of LibsRootDir (flat structure, no subdirectories).
getTarballPathInLibsRootDir :: WaspLib -> Path' (Rel LibsRootDir) File'
getTarballPathInLibsRootDir = tarballFilenameAsRelFile . generatedCodeDirTarballFilename
