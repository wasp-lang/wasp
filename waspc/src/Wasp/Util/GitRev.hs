module Wasp.Util.GitRev
  ( gitRevDescription,
  )
where

import qualified GitHash
import qualified Language.Haskell.TH.Syntax as TH
import System.Environment (lookupEnv)

-- | The `git describe` of the Wasp source tree this binary was built from, or
-- `Nothing` if it couldn't be determined (e.g. building outside of a git
-- repository or without `git` installed). This is embedded at build time
-- through GitHash's usage of Template Haskell.
gitRevDescription :: Maybe String
gitRevDescription =
  either (const buildEnvGitRev) (Just . getGitDescription) $$GitHash.tGitInfoCwdTry
  where
    getGitDescription gitInfo
      | GitHash.giDirty gitInfo = GitHash.giTag gitInfo ++ "-dirty"
      | otherwise = GitHash.giTag gitInfo

-- | Fallback for builds where the git repository isn't available, e.g. the
-- Nix build sandbox: whatever the WASP_BUILD_GIT_REV environment variable
-- holds at build time gets embedded here (or `Nothing` if it's unset).
buildEnvGitRev :: Maybe String
buildEnvGitRev = $(TH.lift =<< TH.runIO (lookupEnv "WASP_BUILD_GIT_REV"))
