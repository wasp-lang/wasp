module Wasp.Generator.NpmInstall.InstalledNpmDepsLog
  ( loadInstalledNpmDepsLog,
    saveInstalledNpmDepsLog,
    forgetInstalledNpmDepsLog,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as B
import StrongPath (Abs, Dir, File', Path', Rel, relfile, (</>))
import qualified StrongPath as SP
import System.Directory (doesFileExist)
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.NpmInstall.Common (AllNpmDeps)
import Wasp.Util.IO (deleteFileIfExists)

-- Load the log of the npm dependencies we installed, from disk.
loadInstalledNpmDepsLog :: Path' Abs (Dir GeneratedAppDir) -> IO (Maybe AllNpmDeps)
loadInstalledNpmDepsLog dstDir = do
  fileExists <- doesFileExist $ SP.fromAbsFile logFilePath
  if fileExists
    then do
      fileContents <- B.readFile $ SP.fromAbsFile logFilePath
      return (Aeson.decode fileContents :: Maybe AllNpmDeps)
    else return Nothing
  where
    logFilePath = getInstalledNpmDepsLogFilePath dstDir

-- Save the record of the Wasp's (webapp + server) npm dependencies we installed, to disk.
saveInstalledNpmDepsLog :: AllNpmDeps -> Path' Abs (Dir GeneratedAppDir) -> IO ()
saveInstalledNpmDepsLog deps dstDir =
  B.writeFile (SP.fromAbsFile $ getInstalledNpmDepsLogFilePath dstDir) (Aeson.encode deps)

forgetInstalledNpmDepsLog :: Path' Abs (Dir GeneratedAppDir) -> IO ()
forgetInstalledNpmDepsLog dstDir =
  deleteFileIfExists $ getInstalledNpmDepsLogFilePath dstDir

getInstalledNpmDepsLogFilePath :: Path' Abs (Dir GeneratedAppDir) -> Path' Abs File'
getInstalledNpmDepsLogFilePath dstDir = dstDir </> installedNpmDepsLogFileInGeneratedAppDir

installedNpmDepsLogFileInGeneratedAppDir :: Path' (Rel GeneratedAppDir) File'
installedNpmDepsLogFileInGeneratedAppDir = [relfile|installedNpmDepsLog.json|]
