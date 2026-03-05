module Wasp.LSP.ServerMonads.HasProjectRootDir
  ( HasProjectRootDir (getProjectRootDir),
  )
where

import qualified StrongPath as SP
import Wasp.Project (WaspProjectDir)

class (Monad m) => HasProjectRootDir m where
  getProjectRootDir :: m (Maybe (SP.Path' SP.Abs (SP.Dir WaspProjectDir)))
