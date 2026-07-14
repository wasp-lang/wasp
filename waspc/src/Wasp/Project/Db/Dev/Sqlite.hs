module Wasp.Project.Db.Dev.Sqlite
  ( defaultDevDbFile,
    ensureStateDir,
  )
where

import StrongPath (Abs, Dir, Path', (</>))
import qualified StrongPath as SP
import System.Directory (createDirectoryIfMissing)
import Wasp.Project.Common
  ( WaspProjectDir,
    stateDirInWaspProjectDir,
  )

defaultDevDbFile :: String
defaultDevDbFile = "file:../../state/dev.db"

ensureStateDir :: Path' Abs (Dir WaspProjectDir) -> IO ()
ensureStateDir waspProjectDir =
  createDirectoryIfMissing True . SP.fromAbsDir $ waspProjectDir </> stateDirInWaspProjectDir
