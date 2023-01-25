module Wasp.Cli.Command.FileSystem
  ( deleteFileIfExists,
    deleteDirectoryIfExists,
    getUserCacheDirPath,
    getHomeDir,
    waspInstallationDir,
    waspBinFile,
    UserCacheDir,
  )
where

import Control.Monad (when)
import Data.Maybe (fromJust)
import StrongPath (Abs, Dir, Dir', File', Path', Rel, reldir, relfile)
import qualified StrongPath as SP
import System.Directory
import qualified System.Directory as SD

data UserCacheDir

data HomeDir

deleteDirectoryIfExists :: FilePath -> IO ()
deleteDirectoryIfExists dirPath = do
  doesDirExist <- SD.doesDirectoryExist dirPath
  when doesDirExist $ SD.removeDirectoryRecursive dirPath

deleteFileIfExists :: FilePath -> IO ()
deleteFileIfExists dirPath = do
  doesDirExist <- SD.doesFileExist dirPath
  when doesDirExist $ SD.removeFile dirPath

getUserCacheDirPath :: IO (Path' Abs (Dir UserCacheDir))
getUserCacheDirPath = SD.getXdgDirectory SD.XdgCache "" >>= SP.parseAbsDir

getHomeDir :: IO (Path' Abs (Dir HomeDir))
getHomeDir = fromJust . SP.parseAbsDir <$> getHomeDirectory

waspInstallationDir :: Path' (Rel HomeDir) Dir'
waspInstallationDir = [reldir|.local/share/wasp-lang|]

waspBinFile :: Path' (Rel HomeDir) File'
waspBinFile = [relfile|.local/bin/wasp|]
