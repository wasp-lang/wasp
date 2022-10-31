module Wasp.AppSpec.ConfigFile
  ( ConfigFileRelocator (..),
  )
where

import StrongPath (Abs, File', Path', Rel)
import Wasp.Generator.Common (ProjectRootDir)

-- | A type for establishing the mapping of where to copy config files from/to.
data ConfigFileRelocator = ConfigFileRelocator
  { _pathInWaspDir :: Path' Abs File',
    _pathInProjectRootDir :: Path' (Rel ProjectRootDir) File'
  }
  deriving (Show, Eq)
