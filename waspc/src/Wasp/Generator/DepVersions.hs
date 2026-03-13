module Wasp.Generator.DepVersions
  ( prismaVersion,
    superjsonVersion,
    typescriptVersion,
    reactVersion,
    reactDomVersion,
    reactTypesVersion,
    reactDomTypesVersion,
    reactRouterVersion,
    reactQueryVersion,
    expressVersionStr,
    expressTypesVersion,
    axiosVersion,
    viteVersion,
    dotenvVersion,
  )
where

import qualified Wasp.SemanticVersion as SV

-- NOTE: If changing prisma version here, also change it in
-- waspc/packages/prisma/package.json. Then ensure
-- `data/Generator/templates/sdk/wasp/prisma-runtime-library.d.ts` is up to
-- date.
prismaVersion :: SV.Version
prismaVersion = SV.Version 5 19 1

superjsonVersion :: SV.Range
superjsonVersion = SV.Range [SV.backwardsCompatibleWith (SV.Version 2 2 1)]

typescriptVersion :: SV.Version
typescriptVersion = SV.Version 5 8 2

-- When updating the React version, also update it in `peerDependencies` in `waspc/libs/auth/package.json`.
reactVersion :: SV.Range
reactVersion = SV.Range [SV.backwardsCompatibleWith (SV.Version 19 2 1)]

-- React and ReactDOM versions should always match.
reactDomVersion :: SV.Range
reactDomVersion = reactVersion

-- Follows React major version
-- When updating the React types version, also update it in `devDependencies` in `waspc/libs/auth/package.json`.
reactTypesVersion :: SV.Range
reactTypesVersion = SV.Range [SV.backwardsCompatibleWith (SV.Version 19 2 7)]

reactDomTypesVersion :: SV.Range
reactDomTypesVersion = SV.Range [SV.backwardsCompatibleWith (SV.Version 19 2 3)]

reactRouterVersion :: SV.Range
reactRouterVersion = SV.Range [SV.backwardsCompatibleWith (SV.Version 7 12 0)]

-- TODO: Update react query and express to use Wasp.SemanticVersion when we'll
-- have support for patch versions https://github.com/wasp-lang/wasp/issues/2941

reactQueryVersion :: String
reactQueryVersion = "~4.42.0"

expressVersionStr :: String
expressVersionStr = "~5.1.0"

expressTypesVersion :: SV.Range
expressTypesVersion = SV.Range [SV.backwardsCompatibleWith (SV.Version 5 0 0)]

axiosVersion :: SV.Range
axiosVersion = SV.Range [SV.backwardsCompatibleWith (SV.Version 1 4 0)]

viteVersion :: SV.Range
viteVersion = SV.Range [SV.backwardsCompatibleWith (SV.Version 7 0 6)]

dotenvVersion :: SV.Range
dotenvVersion = SV.Range [SV.backwardsCompatibleWith (SV.Version 16 6 1)]
