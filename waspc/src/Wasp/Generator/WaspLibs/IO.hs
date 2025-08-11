module Wasp.Generator.WaspLibs.IO
  ( copyWaspLibs,
    setDstTarballVersionToChecksum,
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
import qualified Wasp.Data as Data
import qualified Wasp.ExternalConfig.Npm.Tarball as Npm.Tarball
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.Monad (GeneratorError (GenericGeneratorError))
import Wasp.Generator.WaspLibs.Common (LibsRootDir, LibsSourceDir, libsDirPathInDataDir, libsRootDirInProjectRootDir)
import qualified Wasp.Generator.WaspLibs.WaspLib as WaspLib
import Wasp.Util (checksumFromFilePath, hexToString)
import Wasp.Util.IO (copyFile, deleteDirectoryIfExists)

setDstTarballVersionToChecksum :: WaspLib.WaspLib -> IO WaspLib.WaspLib
setDstTarballVersionToChecksum waspLib = do
  tarballSrcPath <- (</> castRel (WaspLib.srcTarballPath waspLib)) <$> getAbsLibsSourceDirPath
  tarballChecksum <- computeTarballChecksum tarballSrcPath

  return $
    waspLib
      { WaspLib.dstTarballPath =
          Npm.Tarball.makeTarballFilePath
            (WaspLib.tarballName waspLib)
            tarballChecksum
      }
  where
    computeTarballChecksum tarballSrcPath = take 8 . hexToString <$> checksumFromFilePath tarballSrcPath

copyWaspLibs :: Path' Abs (Dir ProjectRootDir) -> [WaspLib.WaspLib] -> IO (Maybe [GeneratorError])
copyWaspLibs projectRootDir waspLibs = do
  libsSrcDirPath <- getAbsLibsSourceDirPath
  libsDstDirPath <- ensureLibsDstDir

  let copyWaspLib = copyWaspLibFromSrcToDst libsSrcDirPath libsDstDirPath
  (mapM_ copyWaspLib waspLibs >> return Nothing)
    `catch` (\e -> return $ Just [GenericGeneratorError $ show (e :: IOError)])
  where
    ensureLibsDstDir :: IO (Path' Abs (Dir LibsRootDir))
    ensureLibsDstDir = do
      let libsDstDirPath = projectRootDir </> libsRootDirInProjectRootDir
      -- Clean up old lib files
      deleteDirectoryIfExists libsDstDirPath
      createDirectory (fromAbsDir libsDstDirPath)
      return libsDstDirPath

    copyWaspLibFromSrcToDst :: Path' Abs (Dir LibsSourceDir) -> Path' Abs (Dir LibsRootDir) -> WaspLib.WaspLib -> IO ()
    copyWaspLibFromSrcToDst srcDir dstDir waspLib = do
      let srcPath = srcDir </> castRel (WaspLib.srcTarballPath waspLib)
          dstPath = dstDir </> castRel (WaspLib.dstTarballPath waspLib)
      copyFile srcPath dstPath

getAbsLibsSourceDirPath :: IO (Path' Abs (Dir LibsSourceDir))
getAbsLibsSourceDirPath = (</> libsDirPathInDataDir) <$> Data.getAbsDataDirPath
