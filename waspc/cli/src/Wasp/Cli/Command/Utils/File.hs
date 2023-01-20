module Wasp.Cli.Command.Utils.File
  ( deleteFileIfExists,
    deleteDirectoryIfExists,
    getUserCacheDirPath,
    UserCacheDir,
  )
where

import Control.Monad (when)
import qualified StrongPath as SP
import qualified System.Directory as SD

deleteDirectoryIfExists :: FilePath -> IO ()
deleteDirectoryIfExists dirPath = do
  doesDirExist <- SD.doesDirectoryExist dirPath
  when doesDirExist $ SD.removeDirectoryRecursive dirPath

deleteFileIfExists :: FilePath -> IO ()
deleteFileIfExists dirPath = do
  doesDirExist <- SD.doesFileExist dirPath
  when doesDirExist $ SD.removeFile dirPath

data UserCacheDir

getUserCacheDirPath :: IO (SP.Path' SP.Abs (SP.Dir UserCacheDir))
getUserCacheDirPath = SD.getXdgDirectory SD.XdgCache "" >>= SP.parseAbsDir
