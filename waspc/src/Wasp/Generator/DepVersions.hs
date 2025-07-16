module Wasp.Generator.DepVersions
  ( prismaVersion,
    superjsonVersion,
    typescriptVersion,
  )
where

import qualified Wasp.SemanticVersion as SV

prismaVersion :: SV.Version
-- NOTE: If changing prisma version here, also change it in waspc/packages/prisma/package.json.
--       Then, make sure `data/Generator/templates/sdk/wasp/prisma-runtime-library.d.ts` is up to date.
prismaVersion = SV.Version 5 19 1

superjsonVersion :: SV.ComparatorSet
superjsonVersion = SV.backwardsCompatibleWith $ SV.Version 2 2 1

typescriptVersion :: SV.Version
typescriptVersion = SV.Version 5 8 2
