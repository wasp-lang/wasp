module Wasp.Cli.Command.FileSystem
  ( getUserCacheDir,
    getWaspCacheDir,
    getHomeDir,
    waspInstallationDirInHomeDir,
    waspExecutableInHomeDir,
    UserCacheDir,
    WaspCacheDir,
  )
where

import Data.Maybe (fromJust)
import StrongPath (Abs, Dir, Dir', File', Path', Rel, reldir, relfile, (</>))
import qualified StrongPath as SP
import System.Directory
import qualified System.Directory as SD

data UserHomeDir

data UserCacheDir

data WaspCacheDir

getHomeDir :: IO (Path' Abs (Dir UserHomeDir))
getHomeDir = fromJust . SP.parseAbsDir <$> getHomeDirectory

getWaspCacheDir :: Path' Abs (Dir UserCacheDir) -> Path' Abs (Dir WaspCacheDir)
getWaspCacheDir userCacheDirPath = userCacheDirPath </> [reldir|wasp|]

getUserCacheDir :: IO (Path' Abs (Dir UserCacheDir))
getUserCacheDir = SD.getXdgDirectory SD.XdgCache "" >>= SP.parseAbsDir

-- NOTE: these paths are based on the installer script and if you change them there
-- you need to change them here as well (and vice versa).
-- Task to improve this: https://github.com/wasp-lang/wasp/issues/980
waspInstallationDirInHomeDir :: Path' (Rel UserHomeDir) Dir'
waspInstallationDirInHomeDir = [reldir|.local/share/wasp-lang|]

waspExecutableInHomeDir :: Path' (Rel UserHomeDir) File'
waspExecutableInHomeDir = [relfile|.local/bin/wasp|]
