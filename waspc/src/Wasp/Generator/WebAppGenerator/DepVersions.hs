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

reactQueryVersion :: SV.ComparatorSet
reactQueryVersion = SV.backwardsCompatibleWith $ SV.Version 4 39 1

axiosVersion :: SV.ComparatorSet
axiosVersion = SV.backwardsCompatibleWith $ SV.Version 1 4 0

reactVersion :: SV.ComparatorSet
reactVersion = SV.backwardsCompatibleWith $ SV.Version 18 2 0

reactTypesVersion :: SV.ComparatorSet
reactTypesVersion = SV.backwardsCompatibleWith $ SV.Version 18 0 37 -- follows React major version

viteVersion :: SV.ComparatorSet
viteVersion = SV.backwardsCompatibleWith $ SV.Version 7 0 6
