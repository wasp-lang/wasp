module Wasp.Generator.WebAppGenerator.DepVersions
  ( reactRouterVersion,
    reactQueryVersion,
    axiosVersion,
    reactVersion,
    reactTypesVersion,
    viteVersion,
  )
where

import qualified Wasp.SemanticVersion as SV

reactRouterVersion :: SV.ComparatorSet
reactRouterVersion = SV.backwardsCompatibleWith $ SV.Version 6 26 2

-- TODO: update this to use Wasp.SemanticVersion when we'll have support for patch versions
-- https://github.com/wasp-lang/wasp/issues/2941
-- also update expressVersionStr
reactQueryVersion :: String
reactQueryVersion = "~4.41.0"

axiosVersion :: SV.ComparatorSet
axiosVersion = SV.backwardsCompatibleWith $ SV.Version 1 4 0

reactVersion :: SV.ComparatorSet
reactVersion = SV.backwardsCompatibleWith $ SV.Version 18 2 0

reactTypesVersion :: SV.ComparatorSet
reactTypesVersion = SV.backwardsCompatibleWith $ SV.Version 18 0 37 -- follows React major version

viteVersion :: SV.ComparatorSet
viteVersion = SV.backwardsCompatibleWith $ SV.Version 7 0 6
