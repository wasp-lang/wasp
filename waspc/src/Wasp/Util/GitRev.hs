module Wasp.Util.GitRev
  ( gitRevDescription,
  )
where

import qualified GitHash
import Wasp.Util (eitherToMaybe)

-- | The `git describe` of the Wasp source tree this binary was built from, or
-- `Nothing` if it couldn't be determined (e.g. building outside of a git
-- repository or without `git` installed). This is embedded at build time
-- through GitHash's usage of Template Haskell.
gitRevDescription :: Maybe String
gitRevDescription =
  eitherToMaybe $ getGitDescription <$> $$GitHash.tGitInfoCwdTry
  where
    getGitDescription gitInfo
      | GitHash.giDirty gitInfo = GitHash.giTag gitInfo ++ "-dirty"
      | otherwise = GitHash.giTag gitInfo
