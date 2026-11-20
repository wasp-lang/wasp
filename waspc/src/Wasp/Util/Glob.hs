module Wasp.Util.Glob
  ( dirGlob,
    descendantsGlob,
  )
where

import StrongPath (Dir, Path', toFilePath)
import System.FilePath.Glob (Pattern)
import System.FilePath.Glob.Primitive (literal, recursiveWildcard, wildcard)

-- | Matches the directory itself. Like the paths of directories, it ends with
-- a path separator.
dirGlob :: Path' b (Dir d) -> Pattern
dirGlob = literal . toFilePath

-- | Matches anything inside the directory, at any depth.
descendantsGlob :: Path' b (Dir d) -> Pattern
descendantsGlob dir = dirGlob dir <> recursiveWildcard <> wildcard
