module Wasp.AppSpec.Inspectable
  ( InspectableAppSpec (..),
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Inspectable (Inspectable (..), InspectionEntry (InspectionEntry))
import qualified Wasp.SemanticVersion as SV

data InspectableAppSpec = InspectableAppSpec
  { getAppSpec :: AS.AppSpec,
    -- | The version of the Wasp that analyzed the app spec.
    getWaspVersion :: SV.Version
  }

instance ToJSON InspectableAppSpec where
  toJSON (InspectableAppSpec spec waspVersion) =
    object
      [ "waspVersion" .= show waspVersion,
        "dbSystem" .= ASV.getValidDbSystem spec,
        "decls" .= AS.decls spec
      ]

instance Inspectable InspectableAppSpec where
  inspect (InspectableAppSpec spec waspVersion) =
    InspectionEntry
      "Spec"
      [ ("Version", show waspVersion),
        ("Database", show (ASV.getValidDbSystem spec))
      ]
      : concatMap inspect (AS.decls spec)
