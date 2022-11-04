module Wasp.Version (waspVersion) where

import qualified Data.Version as DV
import qualified Paths_waspc
import qualified Wasp.SemanticVersion as SV

waspVersion :: SV.Version
waspVersion = SV.Version (toEnum major) (toEnum minor) (toEnum patch)
  where
    DV.Version [major, minor, patch] _ = Paths_waspc.version
