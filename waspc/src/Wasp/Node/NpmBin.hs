module Wasp.Node.NpmBin
  ( findNpmBin,
    getNpmBinEnvVars,
  )
where

import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import StrongPath (Abs, Dir, Path', reldir)
import qualified StrongPath as SP
import System.Directory (findExecutablesInDirectories)
import System.Environment (lookupEnv)
import qualified System.FilePath as FP
import Wasp.Env (EnvVar)

-- | Finds the executable an npm script in the given directory would run for the
-- given name, by looking in the @PATH@ from 'getNpmBinEnvVars'. Returns
-- 'Nothing' if none of its directories has it.
-- We run these executables directly instead of through npm scripts or @npx@,
-- because those run the command through a shell, which doesn't always forward
-- signals to it, so stopping the job could leave the command running.
findNpmBin :: Path' Abs (Dir d) -> String -> IO (Maybe FilePath)
findNpmBin fromDir binName = do
  searchPath <- getNpmBinSearchPath fromDir
  listToMaybe <$> findExecutablesInDirectories searchPath binName

-- | Constructs the env that npm runs scripts with. Mainly we need the @PATH@,
-- which includes the `node_modules/.bin` of this directory and ancestors. Used
-- for running other npx executables from npm scripts. (e.g. @nodemon@ running
-- @prisma@).
getNpmBinEnvVars :: Path' Abs (Dir d) -> IO [EnvVar]
getNpmBinEnvVars fromDir = do
  searchPath <- getNpmBinSearchPath fromDir
  return [("PATH", intercalate [FP.searchPathSeparator] searchPath)]

-- | The @node_modules/.bin@ directory of the given directory and of each of its
-- ancestors, closest first, and then the current @PATH@.
getNpmBinSearchPath :: Path' Abs (Dir d) -> IO [FilePath]
getNpmBinSearchPath fromDir = do
  currentPath <- maybe [] FP.splitSearchPath <$> lookupEnv "PATH"
  return $ npmBinDirs fromDir ++ currentPath

npmBinDirs :: Path' Abs (Dir d) -> [FilePath]
npmBinDirs dir =
  FP.dropTrailingPathSeparator (SP.fromAbsDir $ dir SP.</> [reldir|node_modules/.bin|])
    : (if parent == dir then [] else npmBinDirs parent)
  where
    parent = SP.parent dir
