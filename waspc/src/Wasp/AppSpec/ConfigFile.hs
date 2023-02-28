module Wasp.AppSpec.ConfigFile
  ( ConfigFileRelocator (..),
  )
where

import StrongPath (File', Path', Rel)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Project.Common (WaspProjectDir)

-- | A type for establishing the mapping of where to copy config files from/to.
data ConfigFileRelocator = ConfigFileRelocator
  { _pathInWaspProjectDir :: Path' (Rel WaspProjectDir) File',
    _pathInProjectRootDir :: Path' (Rel ProjectRootDir) File'
  }
  deriving (Show, Eq)
