module Wasp.Generator.WaspLibs.IO
  ( copyWaspLibsToGeneratedProjectDir,
  )
where

import Control.Exception (catch)
import Control.Monad (forM, forM_)
import StrongPath
  ( Abs,
    Dir,
    Path',
    fromAbsDir,
    (</>),
  )
import System.Directory (createDirectory)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.ExternalConfig.Npm.Tarball as Npm.Tarball
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.Monad (GeneratorError (GenericGeneratorError))
import Wasp.Generator.WaspLibs.Common
  ( LibsRootDir,
    LibsSourceDir,
    getAbsLibsSourceDirPath,
    libsRootDirInGeneratedCodeDir,
    libsRootDirNextToSdk,
  )
import qualified Wasp.Generator.WaspLibs.WaspLib as WaspLib
import Wasp.Util.IO (copyFile, deleteDirectoryIfExists)

copyWaspLibsToGeneratedProjectDir :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> IO (Maybe [GeneratorError])
copyWaspLibsToGeneratedProjectDir spec projectRootDir = do
  libsSourceDirPath <- getAbsLibsSourceDirPath

  -- We need to accomodate the SDK hacks with libs, so we copy the libs
  -- differently depending on the context:
  -- 1. When running `wasp start` - copy them only to the `.wasp/out` dir
  -- 2. When running `wasp build` - copy them to the `.wasp/build` AND
  --   `.wasp/out` dir.
  let destinationDirPaths =
        if AS.isBuild spec
          then [libsPathInGeneratedCodeDir, libsPathNextToSdk]
          else [libsPathInGeneratedCodeDir]

  mconcat <$> forM destinationDirPaths (copyWaspLibs waspLibs libsSourceDirPath)
  where
    libsPathInGeneratedCodeDir = projectRootDir </> libsRootDirInGeneratedCodeDir
    libsPathNextToSdk = projectRootDir </> libsRootDirNextToSdk

    waspLibs = AS.waspLibs spec

copyWaspLibs :: [WaspLib.WaspLib] -> Path' Abs (Dir LibsSourceDir) -> Path' Abs (Dir LibsRootDir) -> IO (Maybe [GeneratorError])
copyWaspLibs waspLibs sourceDirPath destinationDirPath = do
  ensureDestinationDir
  let copyWaspLib = copyWaspLibFromSrcToDst sourceDirPath destinationDirPath
  (forM_ waspLibs copyWaspLib >> return Nothing)
    `catch` (\e -> return $ Just [GenericGeneratorError $ show (e :: IOError)])
  where
    ensureDestinationDir :: IO ()
    ensureDestinationDir = do
      -- Clean up old lib files.
      deleteDirectoryIfExists destinationDirPath
      createDirectory (fromAbsDir destinationDirPath)

    copyWaspLibFromSrcToDst :: Path' Abs (Dir LibsSourceDir) -> Path' Abs (Dir LibsRootDir) -> WaspLib.WaspLib -> IO ()
    copyWaspLibFromSrcToDst srcDir dstDir waspLib = do
      let srcPath = srcDir </> Npm.Tarball.filename (WaspLib.dataDirTarball waspLib)
          dstPath = dstDir </> Npm.Tarball.filename (WaspLib.generatedCodeDirTarball waspLib)
      copyFile srcPath dstPath
