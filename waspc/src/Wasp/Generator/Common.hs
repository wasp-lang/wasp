module Wasp.Generator.Common
  ( ProjectRootDir,
    latestMajorNodeVersion,
    nodeVersionRange,
    npmVersionRange,
    prismaVersion,
    GeneratedCodeSourceDir,
  )
where

import qualified Wasp.SemanticVersion as SV

-- | Directory where the whole web app project (client, server, ...) is generated.
data ProjectRootDir

-- | Used as a constraint to specify root directories with generated code (e.g. web app src dir or server src dir).
class GeneratedCodeSourceDir d

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

-- | Range of npm versions that Wasp and generated projects work correctly with.
npmVersionRange :: SV.Range
npmVersionRange = SV.Range [SV.backwardsCompatibleWith latestLTSVersion]
  where
    latestLTSVersion = SV.Version 8 19 2 -- Goes with node 18 (but also higher versions too).

prismaVersion :: SV.Version
prismaVersion = SV.Version 4 5 0
