module Wasp.Generator.ServerGenerator.DepVersions
  ( expressVersionStr,
    expressTypesVersion,
  )
where

import qualified Wasp.SemanticVersion as SV

-- TODO: update this to use Wasp.SemanticVersion when we'll have support for patch versions
-- https://github.com/wasp-lang/wasp/issues/2941
-- also update reactQueryVersion
expressVersionStr :: String
expressVersionStr = "~5.1.0"

expressTypesVersion :: SV.ComparatorSet
expressTypesVersion = SV.backwardsCompatibleWith $ SV.Version 5 0 0
