module Wasp.Project.ExternalConfig.ExternalConfigAnalysisContext
  ( ExternalConfigAnalysisContext (..),
  )
where

import StrongPath (Abs, Dir, File, Path', Rel)
import Wasp.AppSpec.ConfigFile (ConfigFileRelocator)
import Wasp.Project.Common
  ( SrcTsConfigFile,
    WaspProjectDir,
  )

data ExternalConfigAnalysisContext = ExternalConfigAnalysisContext
  { _tailwindConfigFilesRelocators :: [ConfigFileRelocator],
    _waspDir :: Path' Abs (Dir WaspProjectDir),
    _srcTsConfigPath :: Path' (Rel WaspProjectDir) (File SrcTsConfigFile)
  }
