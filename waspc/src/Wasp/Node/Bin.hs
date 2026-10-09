module Wasp.Node.Bin
  ( nodeBinProc,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List (intercalate)
import Data.Maybe (fromMaybe, listToMaybe)
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import System.Directory (findExecutablesInDirectories)
import System.Environment (lookupEnv)
import qualified System.FilePath as FP
import qualified System.Process as P
import Wasp.Env (EnvVar, inheritEnvWith)

-- | Like 'P.proc', but runs the command the way an npm script in the given
-- directory would, with the current process's env vars combined with the given
-- ones. It looks for the executable in the @node_modules/.bin@ directory of the
-- given directory and of each of its ancestors, and then in @PATH@. The process
-- also gets those directories prepended to its @PATH@, so that it can run
-- executables from them too.
-- We don't use npm scripts or @npx@ themselves because they run the command
-- through a shell, which doesn't always forward signals to it, so stopping the
-- job could leave the command running.
nodeBinProc :: (MonadIO m) => [EnvVar] -> Path' Abs (Dir a) -> String -> [String] -> m P.CreateProcess
nodeBinProc extraEnvVars fromDir binName args = liftIO $ do
  path <- (nodeBinDirs ++) . maybe [] FP.splitSearchPath <$> lookupEnv "PATH"
  binPath <- fromMaybe binName . listToMaybe <$> findExecutablesInDirectories nodeBinDirs binName
  inheritEnvWith
    (("PATH", intercalate [FP.searchPathSeparator] path) : extraEnvVars)
    (P.proc binPath args) {P.cwd = Just $ SP.fromAbsDir fromDir}
  where
    nodeBinDirs = (FP.</> "node_modules" FP.</> ".bin") <$> ancestorDirs (FP.dropTrailingPathSeparator $ SP.fromAbsDir fromDir)
    ancestorDirs dir =
      let parentDir = FP.takeDirectory dir
       in dir : if parentDir == dir then [] else ancestorDirs parentDir
