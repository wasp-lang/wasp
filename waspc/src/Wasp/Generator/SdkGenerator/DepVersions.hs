module Wasp.Generator.SdkGenerator.DepVersions
  ( tailwindCssVersion,
  )
where

import qualified Wasp.SemanticVersion as SV

tailwindCssVersion :: SV.ComparatorSet
tailwindCssVersion = SV.backwardsCompatibleWith $ SV.Version 3 2 7
