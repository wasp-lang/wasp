module Wasp.Job.CreateProcess.SearchPath
  ( findExecutableFrom,
    findExecutableInSearchPath,
    getSearchPathEnvVars,
  )
where

import Data.List (intercalate)
import StrongPath (Abs, Dir, Path', reldir)
import qualified StrongPath as SP
import System.Directory (findExecutablesInDirectories)
import System.Environment (lookupEnv)
import qualified System.FilePath as FP
import Wasp.Env (EnvVar)

-- | Finds the executable an npm script in the given directory would run for the
-- given name, by looking in the directories from 'getSearchPath'. Fails if none
-- of them has it.
-- We run these executables directly instead of through npm scripts or @npx@,
-- because those run the command through a shell, which doesn't always forward
-- signals to it, so stopping the job could leave the command running.
findExecutableFrom :: Path' Abs (Dir d) -> String -> IO FilePath
findExecutableFrom fromDir executableName =
  getSearchPath fromDir >>= findExecutableInSearchPath fromDir executableName

-- | Like 'findExecutableFrom', but looks in the given search path. Relative
-- entries in it (on POSIX, an empty @PATH@ entry becomes @.@) are resolved
-- against the given directory, like the npm script's shell would, not against
-- Wasp's working directory.
findExecutableInSearchPath :: Path' Abs (Dir d) -> String -> [FilePath] -> IO FilePath
findExecutableInSearchPath fromDir executableName searchPath =
  findExecutablesInDirectories (FP.normalise . (SP.fromAbsDir fromDir FP.</>) <$> searchPath) executableName
    >>= \case
      executable : _ -> return executable
      [] ->
        fail $
          "Could not find `" <> executableName <> "` in node_modules/.bin of " <> SP.fromAbsDir fromDir <> " or its parents, or in the PATH."

-- | The env vars that give a process in the given directory the @PATH@ npm
-- gives its scripts, so that the executables it runs are found the same way
-- as with 'findExecutableFrom'.
getSearchPathEnvVars :: Path' Abs (Dir d) -> IO [EnvVar]
getSearchPathEnvVars fromDir = do
  searchPath <- getSearchPath fromDir
  return [("PATH", intercalate [FP.searchPathSeparator] searchPath)]

-- | The @node_modules/.bin@ directory of the given directory and of each of its
-- ancestors, closest first, and then the current @PATH@.
getSearchPath :: Path' Abs (Dir d) -> IO [FilePath]
getSearchPath fromDir = do
  currentPath <- maybe [] FP.splitSearchPath <$> lookupEnv "PATH"
  return $ npmBinDirs fromDir ++ currentPath

npmBinDirs :: Path' Abs (Dir d) -> [FilePath]
npmBinDirs dir =
  FP.dropTrailingPathSeparator (SP.fromAbsDir $ dir SP.</> [reldir|node_modules/.bin|])
    : (if parent == dir then [] else npmBinDirs parent)
  where
    parent = SP.parent dir
