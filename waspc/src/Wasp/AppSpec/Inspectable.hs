module Wasp.AppSpec.Inspectable
  ( InspectableAppSpec (..),
  )
where

import Wasp.AppSpec (AppSpec (decls))
import Wasp.AppSpec.Core.Inspectable (Inspectable (..), InspectionEntry (InspectionEntry))
import qualified Wasp.AppSpec.Valid as ASV
import qualified Wasp.Version

newtype InspectableAppSpec = InspectableAppSpec {getAppSpec :: AppSpec}

instance Inspectable InspectableAppSpec where
  inspect (InspectableAppSpec spec) =
    InspectionEntry
      "Spec"
      [ ("Version", show Wasp.Version.waspVersion),
        ("Database", show (ASV.getValidDbSystem spec))
      ]
      : concatMap inspect (decls spec)
