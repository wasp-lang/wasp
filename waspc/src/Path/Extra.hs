module Path.Extra
  ( reversePosixPath,
    toPosixFilePath,
  )
where

import Control.Exception (assert)
import Path
import qualified System.FilePath.Posix as FPP

-- | For given posix path P, returns posix path P', such that (terminal pseudocode incoming)
-- `pwd == (cd P && cd P' && pwd)`, or to put it differently, such that
-- `cd P && cd P'` is noop (does nothing).
-- It will always be either "." (only if input is ".") or a series of ".."
-- (e.g. reversePath "foo/bar" == "../..").
reversePosixPath :: FilePath -> FilePath
reversePosixPath path
  | null parts = "."
  | otherwise =
    assert (".." `notElem` parts) $
      FPP.joinPath $ map (const "..") parts
  where
    parts :: [String]
    parts = filter (/= ".") $ FPP.splitDirectories path

toPosixFilePath :: Path Rel a -> FilePath
toPosixFilePath path = map (\c -> if c == '\\' then '/' else c) $ toFilePath path
