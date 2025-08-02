module Wasp.Generator.WaspLibs
  ( copyWaspLibs,
    waspLibs,
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
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.Monad (GeneratorError (GenericGeneratorError))
import Wasp.Generator.WaspLibs.Common (LibsRootDir, LibsSourceDir, libsDirPathInDataDir, libsRootDirInProjectRootDir)
import qualified Wasp.Generator.WaspLibs.WaspLib as WaspLib
import Wasp.Util.IO (copyFile, deleteDirectoryIfExists)

waspLibs :: [WaspLib.WaspLib]
waspLibs =
  [ WaspLib.makeWaspLib "@wasp.sh/libs-auth"
  ]

copyWaspLibs :: Path' Abs (Dir ProjectRootDir) -> IO [GeneratorError]
copyWaspLibs projectRootDir = do
  libsSrcPath <- (</> libsDirPathInDataDir) <$> Data.getAbsDataDirPath
  libsDstPath <- ensureLibsDstDir projectRootDir

  let copyWaspLib = copyWaspLibFromSrcToDst libsSrcPath libsDstPath
  (mapM_ copyWaspLib waspLibs >> return [])
    `catch` (\e -> return [GenericGeneratorError $ show (e :: IOError)])

ensureLibsDstDir :: Path' Abs (Dir ProjectRootDir) -> IO (Path' Abs (Dir LibsRootDir))
ensureLibsDstDir projectRootDir = do
  let libsDstPath = projectRootDir </> libsRootDirInProjectRootDir
  -- Clean up old lib files
  deleteDirectoryIfExists libsDstPath
  createDirectory (fromAbsDir libsDstPath)
  return libsDstPath

copyWaspLibFromSrcToDst ::
  Path' Abs (Dir LibsSourceDir) ->
  Path' Abs (Dir LibsRootDir) ->
  WaspLib.WaspLib ->
  IO ()
copyWaspLibFromSrcToDst srcDir dstDir waspLib = do
  let tarballRelFile = WaspLib.tarballRelFile waspLib
      srcPath = srcDir </> castRel tarballRelFile
      dstPath = dstDir </> castRel tarballRelFile
  copyFile srcPath dstPath
