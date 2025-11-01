module Wasp.Version (waspVersion) where

import qualified Data.Version as DV
import qualified Paths_waspc
import qualified Wasp.SemanticVersion as SV

waspVersion :: SV.Version
waspVersion = case Paths_waspc.version of
  DV.Version [major, minor, patch] _ -> SV.Version (toEnum major) (toEnum minor) (toEnum patch)
  _ -> error "This should never happen. Wasp binary version must have exactly three digits, but it doesn't."
