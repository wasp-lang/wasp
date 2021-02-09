module Data
    ( DataDir
    , getAbsDataDirPath
    ) where

import           StrongPath  (Abs, Dir, Path)
import qualified StrongPath  as SP

import qualified Paths_waspc


data DataDir

getAbsDataDirPath :: IO (Path Abs (Dir DataDir))
getAbsDataDirPath = Paths_waspc.getDataDir >>= SP.parseAbsDir
