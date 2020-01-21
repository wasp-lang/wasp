module Path.Extra
    ( (./)
    ) where

import System.FilePath as FP
import Path


-- NOTE: Here we return FilePath, because Path can't keep the "./" in it,
--   since it always normalizes the path. So the only way to have path with "./" in it
--   is to have it as a FilePath.
(./) :: Path Rel a -> FP.FilePath
(./) relPath = "." FP.</> (toFilePath relPath)
