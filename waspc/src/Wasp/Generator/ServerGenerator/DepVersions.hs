module Wasp.Generator.ServerGenerator.DepVersions
  ( expressVersionStr,
    expressTypesVersion,
  )
where

import qualified Wasp.SemanticVersion as SV

-- TODO: update this to use Wasp.SemanticVersion when we'll have support
-- for patch versions
expressVersionStr :: String
expressVersionStr = "~4.21.0"

expressTypesVersion :: SV.ComparatorSet
expressTypesVersion = SV.backwardsCompatibleWith $ SV.Version 4 17 13
