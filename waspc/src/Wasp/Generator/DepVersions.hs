module Wasp.Generator.DepVersions
  ( -- General
    prismaVersion,
    superjsonVersion,
    typescriptVersion,
    -- React ecosystem
    reactVersion,
    reactTypesVersion,
    reactDomTypesVersion,
    reactRouterVersion,
    reactQueryVersion,
    -- Express
    expressVersionStr,
    expressTypesVersion,
    -- Other
    axiosVersion,
    viteVersion,
    tailwindCssVersion,
  )
where

import qualified Wasp.SemanticVersion as SV

-- NOTE: If changing prisma version here, also change it in
-- waspc/packages/prisma/package.json. Then ensure
-- `data/Generator/templates/sdk/wasp/prisma-runtime-library.d.ts` is up to
-- date.
prismaVersion :: SV.Version
prismaVersion = SV.Version 5 19 1

superjsonVersion :: SV.ComparatorSet
superjsonVersion = SV.backwardsCompatibleWith $ SV.Version 2 2 1

typescriptVersion :: SV.Version
typescriptVersion = SV.Version 5 8 2

reactVersion :: SV.ComparatorSet
reactVersion = SV.backwardsCompatibleWith $ SV.Version 18 2 0

reactTypesVersion :: SV.ComparatorSet
reactTypesVersion = SV.backwardsCompatibleWith $ SV.Version 18 0 37 -- follows React major version

reactDomTypesVersion :: SV.ComparatorSet
reactDomTypesVersion = SV.backwardsCompatibleWith $ SV.Version 18 0 11

reactRouterVersion :: SV.ComparatorSet
reactRouterVersion = SV.backwardsCompatibleWith $ SV.Version 6 26 2

-- TODO: Update react query and express to use Wasp.SemanticVersion when we'll
-- have support for patch versions https://github.com/wasp-lang/wasp/issues/2941

reactQueryVersion :: String
reactQueryVersion = "~4.41.0"

expressVersionStr :: String
expressVersionStr = "~5.1.0"

expressTypesVersion :: SV.ComparatorSet
expressTypesVersion = SV.backwardsCompatibleWith $ SV.Version 5 0 0

axiosVersion :: SV.ComparatorSet
axiosVersion = SV.backwardsCompatibleWith $ SV.Version 1 4 0

viteVersion :: SV.ComparatorSet
viteVersion = SV.backwardsCompatibleWith $ SV.Version 7 0 6

tailwindCssVersion :: SV.ComparatorSet
tailwindCssVersion = SV.backwardsCompatibleWith $ SV.Version 3 4 17
