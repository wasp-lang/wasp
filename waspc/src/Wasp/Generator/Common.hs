module Wasp.Generator.Common
  ( ProjectRootDir,
    latestMajorNodeVersion,
    nodeVersionRange,
    prismaVersion,
  )
where

import qualified Wasp.SemanticVersion as SV

-- | Directory where the whole web app project (client, server, ...) is generated.
data ProjectRootDir

-- | Latest concrete major node version supported by the nodeVersionRange, and
--   therefore by Wasp.
--   Here we assume that nodeVersionRange is using latestNodeLTSVersion as its basis.
--   TODO: instead of making assumptions, extract the latest major node version
--   directly from the nodeVersionRange.
latestMajorNodeVersion :: SV.Version
latestMajorNodeVersion = latestNodeLTSVersion

nodeVersionRange :: SV.Range
nodeVersionRange = SV.Range [SV.backwardsCompatibleWith latestNodeLTSVersion]

latestNodeLTSVersion :: SV.Version
latestNodeLTSVersion = SV.Version 18 12 0

prismaVersion :: SV.Version
prismaVersion = SV.Version 4 5 0
