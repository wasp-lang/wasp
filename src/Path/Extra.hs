module Path.Extra
    ( (./)
    , reversePath
    ) where

import Control.Exception (assert)
import System.FilePath as FP
import Path


-- NOTE: Here we return FilePath, because Path can't keep the "./" in it,
--   since it always normalizes the path. So the only way to have path with "./" in it
--   is to have it as a FilePath.
(./) :: Path Rel a -> FP.FilePath
(./) relPath = "." FP.</> (toFilePath relPath)

-- | For given path P, returns path P', such that (terminal pseudocode incoming)
-- `pwd == (cd P && cd P' && pwd)`, or to put it differently, such that
-- `cd P && cd P'` is noop (does nothing).
-- It will always be either "." (only if input is ".") or a series of ".."
-- (e.g. reversePath [reldir|foo/bar|] == "../..").
reversePath :: Path Rel Dir -> FilePath
reversePath path
    | length parts == 0 = "."
    | otherwise         = assert (not (".." `elem` parts)) $
                          FP.joinPath $ map (const "..") parts
  where
    parts :: [String]
    parts = filter (not . (== ".")) $ FP.splitDirectories $ toFilePath path
