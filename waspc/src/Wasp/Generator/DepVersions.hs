module Wasp.Generator.DepVersions
  ( prismaVersionRange,
    superjsonVersionRange,
    typescriptVersionRange,
    reactVersionRange,
    reactDomVersionRange,
    reactTypesVersionRange,
    reactDomTypesVersionRange,
    reactRouterVersionRange,
    reactQueryVersionRange,
    expressVersionRange,
    expressTypesVersionRange,
    viteVersionRange,
    vitestVersionRange,
    dotenvVersionRange,
    kyVersionRange,
    nitroVersion,
  )
where

import qualified Wasp.SemanticVersion as SV

-- When updating the value, also update it in `waspc/packages/prisma/package.json`.
-- Also ensure `data/Generator/templates/sdk/wasp/prisma-runtime-library.d.ts` is up to date.
prismaVersionRange :: SV.Range
prismaVersionRange = [SV.r|5.19.1|]

superjsonVersionRange :: SV.Range
superjsonVersionRange = [SV.r|^2.2.1|]

typescriptVersionRange :: SV.Range
typescriptVersionRange = [SV.r|6.0.3|]

-- When updating the React version,
-- also update it in `peerDependencies` in `waspc/libs/auth/package.json`.
reactVersionRange :: SV.Range
reactVersionRange = [SV.r|^19.2.1|]

-- React and ReactDOM versions should always match.
reactDomVersionRange :: SV.Range
reactDomVersionRange = reactVersionRange

-- Follows React major version.
-- When updating the value, also update it in `devDependencies` in `waspc/libs/auth/package.json`.
reactTypesVersionRange :: SV.Range
reactTypesVersionRange = [SV.r|^19.2.7|]

reactDomTypesVersionRange :: SV.Range
reactDomTypesVersionRange = [SV.r|^19.2.3|]

reactRouterVersionRange :: SV.Range
reactRouterVersionRange = [SV.r|^8.0.1|]

reactQueryVersionRange :: SV.Range
reactQueryVersionRange = [SV.r|~4.42.0|]

expressVersionRange :: SV.Range
expressVersionRange = [SV.r|~5.1.0|]

expressTypesVersionRange :: SV.Range
expressTypesVersionRange = [SV.r|^5.0.0|]

viteVersionRange :: SV.Range
viteVersionRange = [SV.r|^8.1.0|]

vitestVersionRange :: SV.Range
vitestVersionRange = [SV.r|^4.1.9|]

dotenvVersionRange :: SV.Range
dotenvVersionRange = [SV.r|^16.6.1|]

kyVersionRange :: SV.Range
kyVersionRange = [SV.r|^2.0.0|]

-- Nitro v3 is still in beta and every release carries a pre-release tag, which
-- our 'SV.Range' parser doesn't support (and which we wouldn't want to widen
-- into a range anyway: beta releases are free to break). So we pin the exact
-- version as a plain string.
nitroVersion :: String
nitroVersion = "3.0.260610-beta"
