module Wasp.Generator.WaspLibs.IO
  ( copyWaspLibs,
  )
where

import Control.Exception (catch)
import StrongPath
  ( Abs,
    Dir,
    Path',
    castRel,
    fromAbsDir,
    (</>),
  )
import System.Directory (createDirectory)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
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

copyWaspLibs :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> [WaspLib.WaspLib] -> IO (Maybe [GeneratorError])
copyWaspLibs spec dstDirPath waspLibs = do
  libsSrcDirPath <- getAbsLibsSourceDirPath

  results <- mapM (copyWaspLibsFromSrcToDst waspLibs libsSrcDirPath) libsDstDirPaths
  return $ mconcat results
  where
    libsDstDirPaths =
      -- Always copy libs to the out dir and optionally to the build dir
      dstDirPath </> libsRootDirNextToSdk : [dstDirPath </> libsRootDirInGeneratedCodeDir | AS.isBuild spec]

copyWaspLibsFromSrcToDst :: [WaspLib.WaspLib] -> Path' Abs (Dir LibsSourceDir) -> Path' Abs (Dir LibsRootDir) -> IO (Maybe [GeneratorError])
copyWaspLibsFromSrcToDst waspLibs libsSrcDirPath libsDstDirPath = do
  ensureLibsDstDir

  let copyWaspLib = copyWaspLibFromSrcToDst libsSrcDirPath libsDstDirPath
  (mapM_ copyWaspLib waspLibs >> return Nothing)
    `catch` (\e -> return $ Just [GenericGeneratorError $ show (e :: IOError)])
  where
    ensureLibsDstDir :: IO ()
    ensureLibsDstDir = do
      -- Clean up old lib files
      deleteDirectoryIfExists libsDstDirPath
      createDirectory (fromAbsDir libsDstDirPath)

    copyWaspLibFromSrcToDst :: Path' Abs (Dir LibsSourceDir) -> Path' Abs (Dir LibsRootDir) -> WaspLib.WaspLib -> IO ()
    copyWaspLibFromSrcToDst srcDir dstDir waspLib = do
      let srcPath = srcDir </> castRel (WaspLib.srcTarballPath waspLib)
          dstPath = dstDir </> castRel (WaspLib.dstTarballPath waspLib)
      copyFile srcPath dstPath
