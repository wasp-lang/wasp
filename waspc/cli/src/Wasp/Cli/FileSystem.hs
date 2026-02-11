module Wasp.Cli.FileSystem
  ( getUserCacheDir,
    getWaspCacheDir,
    getHomeDir,
    waspInstallationDirInHomeDir,
    waspExecutableInHomeDir,
    getAbsPathToDirInCwd,
    withTempDir,
    UserCacheDir,
    WaspCacheDir,
  )
where

import Control.Monad.Catch (MonadThrow)
import Data.Maybe (fromJust)
import StrongPath (Abs, Dir, Dir', File', Path', Rel, reldir, relfile, (</>))
import qualified StrongPath as SP
import qualified System.Directory as SD
import qualified System.FilePath as FP
import System.IO.Temp (withSystemTempDirectory)

data UserHomeDir

data UserCacheDir

data WaspCacheDir

getHomeDir :: IO (Path' Abs (Dir UserHomeDir))
getHomeDir = fromJust . SP.parseAbsDir <$> SD.getHomeDirectory

getWaspCacheDir :: Path' Abs (Dir UserCacheDir) -> Path' Abs (Dir WaspCacheDir)
getWaspCacheDir userCacheDirPath = userCacheDirPath </> [reldir|wasp|]

getUserCacheDir :: IO (Path' Abs (Dir UserCacheDir))
getUserCacheDir = SD.getXdgDirectory SD.XdgCache "" >>= SP.parseAbsDir

withTempDir :: (Path' Abs (Dir r) -> IO a) -> IO a
withTempDir action = withSystemTempDirectory ".wasp" (action . fromJust . SP.parseAbsDir)

-- These paths are also defined in:
-- - https://github.com/wasp-lang/wasp/blob/main/scripts/make-npm-packages/templates/main-package/preinstall.js
-- - https://github.com/wasp-lang/get-wasp-sh/blob/master/installer.sh
-- TODO: Do not hardcode: https://github.com/wasp-lang/wasp/issues/980
waspInstallationDirInHomeDir :: Path' (Rel UserHomeDir) Dir'
waspInstallationDirInHomeDir = [reldir|.local/share/wasp-lang|]

waspExecutableInHomeDir :: Path' (Rel UserHomeDir) File'
waspExecutableInHomeDir = [relfile|.local/bin/wasp|]

getAbsPathToDirInCwd :: (MonadThrow m) => String -> IO (m (Path' Abs (Dir d)))
getAbsPathToDirInCwd dirName = do
  absCwd <- SD.getCurrentDirectory
  return $ SP.parseAbsDir $ absCwd FP.</> dirName
