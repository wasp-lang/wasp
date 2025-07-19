module Wasp.Cli.Util.Common
  ( getWorkingDir,
    WorkingDir,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import StrongPath (Abs, Path')
import qualified StrongPath as SP
import System.Directory (getCurrentDirectory)

data WorkingDir

getWorkingDir :: MonadIO m => m (Path' Abs (SP.Dir WorkingDir))
getWorkingDir = liftIO (getCurrentDirectory >>= SP.parseAbsDir)
