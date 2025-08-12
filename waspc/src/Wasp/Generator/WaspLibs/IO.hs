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
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.Monad (GeneratorError (GenericGeneratorError))
import Wasp.Generator.WaspLibs.Common (LibsRootDir, LibsSourceDir, getAbsLibsSourceDirPath, libsRootDirInProjectRootDir)
import qualified Wasp.Generator.WaspLibs.WaspLib as WaspLib
import Wasp.Util.IO (copyFile, deleteDirectoryIfExists)

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
