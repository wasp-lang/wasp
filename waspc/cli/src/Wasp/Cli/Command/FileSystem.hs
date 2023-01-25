module Wasp.Cli.Command.FileSystem
  ( deleteFileIfExists,
    deleteDirectoryIfExists,
    getUserCacheDirPath,
    getHomeDir,
    waspInstallationDirInHomeDir,
    waspExecutableInHomeDir,
    UserCacheDir,
  )
where

import Control.Monad (when)
import Data.Maybe (fromJust)
import StrongPath (Abs, Dir, Dir', File, File', Path, Path', Rel, reldir, relfile)
import qualified StrongPath as SP
import System.Directory
import qualified System.Directory as SD

data UserCacheDir

data HomeDir

deleteDirectoryIfExists :: Path a b (Dir c) -> IO ()
deleteDirectoryIfExists dirPath = do
  let dirPathStr = SP.toFilePath dirPath
  exists <- SD.doesDirectoryExist dirPathStr
  when exists $ SD.removeDirectoryRecursive dirPathStr

deleteFileIfExists :: Path a b (File c) -> IO ()
deleteFileIfExists filePath = do
  let filePathStr = SP.toFilePath filePath
  exists <- SD.doesFileExist filePathStr
  when exists $ SD.removeFile filePathStr

getUserCacheDirPath :: IO (Path' Abs (Dir UserCacheDir))
getUserCacheDirPath = SD.getXdgDirectory SD.XdgCache "" >>= SP.parseAbsDir

getHomeDir :: IO (Path' Abs (Dir HomeDir))
getHomeDir = fromJust . SP.parseAbsDir <$> getHomeDirectory

waspInstallationDirInHomeDir :: Path' (Rel HomeDir) Dir'
waspInstallationDirInHomeDir = [reldir|.local/share/wasp-lang|]

waspExecutableInHomeDir :: Path' (Rel HomeDir) File'
waspExecutableInHomeDir = [relfile|.local/bin/wasp|]
