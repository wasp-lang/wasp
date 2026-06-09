module Wasp.Util.GitRev
  ( gitRevDescription,
  )
where

import qualified GitHash as GH
import Wasp.Util (eitherToMaybe)

-- | The `git describe` of the Wasp source tree this binary was built from, or
-- `Nothing` if it couldn't be determined (e.g. building outside of a git
-- repository or without `git` installed). This is embedded at build time.
gitRevDescription :: Maybe String
gitRevDescription =
  eitherToMaybe $ getGitDescription <$> $$GH.tGitInfoCwdTry
  where
    getGitDescription gitInfo
      | GH.giDirty gitInfo = GH.giTag gitInfo ++ "-dirty"
      | otherwise = GH.giTag gitInfo
