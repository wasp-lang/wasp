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
reactQueryVersion = SV.backwardsCompatibleWith $ SV.Version 4 29 0

axiosVersion :: SV.ComparatorSet
axiosVersion = SV.backwardsCompatibleWith $ SV.Version 1 4 0

reactVersion :: SV.ComparatorSet
reactVersion = SV.backwardsCompatibleWith $ SV.Version 19 1 0

reactTypesVersion :: SV.ComparatorSet
reactTypesVersion = SV.backwardsCompatibleWith $ SV.Version 19 1 2 -- follows React major version

viteVersion :: SV.ComparatorSet
viteVersion = SV.backwardsCompatibleWith $ SV.Version 4 3 9
