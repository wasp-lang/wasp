module Wasp.Generator.WaspLibs.IO
  ( copyWaspLibs,
    appendChecksumToWaspLibs,
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

copyWaspLibs :: Path' Abs (Dir ProjectRootDir) -> [WaspLib.WaspLib] -> IO (Maybe [GeneratorError])
copyWaspLibs projectRootDir waspLibs = do
  libsSrcDirPath <- getAbsLibsSourceDirPath
  libsDstDirPath <- ensureLibsDstDir projectRootDir

  let copyWaspLib = copyWaspLibFromSrcToDst libsSrcDirPath libsDstDirPath
  (mapM_ copyWaspLib waspLibs >> return Nothing)
    `catch` (\e -> return $ Just [GenericGeneratorError $ show (e :: IOError)])

ensureLibsDstDir :: Path' Abs (Dir ProjectRootDir) -> IO (Path' Abs (Dir LibsRootDir))
ensureLibsDstDir projectRootDir = do
  let libsDstDirPath = projectRootDir </> libsRootDirInProjectRootDir
  -- Clean up old lib files
  deleteDirectoryIfExists libsDstDirPath
  createDirectory (fromAbsDir libsDstDirPath)
  return libsDstDirPath

copyWaspLibFromSrcToDst ::
  Path' Abs (Dir LibsSourceDir) ->
  Path' Abs (Dir LibsRootDir) ->
  WaspLib.WaspLib ->
  IO ()
copyWaspLibFromSrcToDst srcDir dstDir waspLib = do
  let srcPath = srcDir </> castRel (WaspLib.srcTarballPath waspLib)
      dstPath = dstDir </> castRel (WaspLib.dstTarballPath waspLib)
  copyFile srcPath dstPath

appendChecksumToWaspLibs :: [WaspLib.WaspLib] -> IO [WaspLib.WaspLib]
appendChecksumToWaspLibs = mapM appendChecksumToWaspLib
  where
    appendChecksumToWaspLib waspLib = do
      tarballSrcPath <- (</> castRel (WaspLib.srcTarballPath waspLib)) <$> getAbsLibsSourceDirPath
      tarballChecksum <- take 8 . hexToString <$> checksumFromFilePath tarballSrcPath

      return $ appendChecksumToWaspLibTarballPath waspLib tarballChecksum

    -- Converts <name>-<version>.tgz to <name>-<checksum>.tgz
    appendChecksumToWaspLibTarballPath waspLib checksum = waspLib {WaspLib.dstTarballPath = dstTarballPath}
      where
        dstTarballPath = Npm.Tarball.makeTarballFilePath tarballName checksum
        -- dstTarballName = Npm.Tarball.sanitizeForTarballFilename $ show tarballName <> "-" <> checksum
        tarballName = WaspLib.tarballName waspLib

getAbsLibsSourceDirPath :: IO (Path' Abs (Dir LibsSourceDir))
getAbsLibsSourceDirPath = (</> libsDirPathInDataDir) <$> Data.getAbsDataDirPath
