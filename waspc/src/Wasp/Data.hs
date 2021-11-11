module Wasp.Data
  ( DataDir,
    getAbsDataDirPath,
  )
where

import qualified Paths_waspc
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP

data DataDir

getAbsDataDirPath :: IO (Path' Abs (Dir DataDir))
getAbsDataDirPath = Paths_waspc.getDataDir >>= SP.parseAbsDir
