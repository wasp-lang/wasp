module Wasp.Node.Bin
  ( findNpmBin,
    NpmBinNotFound (..),
  )
where

import Control.Exception (Exception (displayException))
import Data.Maybe (listToMaybe)
import StrongPath (Abs, Dir, Path', reldir)
import qualified StrongPath as SP
import System.Directory (findExecutablesInDirectories)

-- | Finds the executable an npm script in the given directory would run for the
-- given name, by looking in the @node_modules/.bin@ directory of the given
-- directory and of each of its ancestors, closest first. Returns 'Nothing' if
-- none of them has it.
-- We run these executables directly instead of through npm scripts or @npx@,
-- because those run the command through a shell, which doesn't always forward
-- signals to it, so stopping the job could leave the command running.
findNpmBin :: Path' Abs (Dir d) -> String -> IO (Maybe FilePath)
findNpmBin fromDir binName = listToMaybe <$> findExecutablesInDirectories lookupDirs binName
  where
    lookupDirs = SP.fromAbsDir <$> makeLookupDirs fromDir

    makeLookupDirs :: Path' Abs (Dir d) -> [Path' Abs (Dir ())]
    makeLookupDirs dir =
      let parent = SP.parent dir
       in (dir SP.</> [reldir|node_modules/.bin|])
            : (if parent == dir then [] else makeLookupDirs parent)

-- | The executable with the given name isn't in any of the @node_modules/.bin@
-- directories 'findNpmBin' looks in from the given directory, e.g. because the
-- project's npm dependencies aren't installed.
data NpmBinNotFound = NpmBinNotFound
  { _npmBinName :: String,
    _npmBinSearchedFromDir :: Path' Abs (Dir ())
  }
  deriving (Eq)

-- | GHC prints uncaught exceptions with 'show', so this shows the message.
instance Show NpmBinNotFound where
  show = displayException

instance Exception NpmBinNotFound where
  displayException (NpmBinNotFound binName fromDir) =
    "Could not find `" <> binName <> "` in node_modules/.bin of " <> SP.fromAbsDir fromDir <> " or its parents."
