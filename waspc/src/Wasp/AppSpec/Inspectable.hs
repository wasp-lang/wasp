module Wasp.AppSpec.Inspectable
  ( InspectableAppSpec (..),
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Inspectable (Inspectable (..), InspectionEntry (InspectionEntry))
import Wasp.Version (waspVersion)

newtype InspectableAppSpec
  = InspectableAppSpec {getAppSpec :: AS.AppSpec}

instance ToJSON InspectableAppSpec where
  toJSON (InspectableAppSpec spec) =
    object
      [ "waspVersion" .= show waspVersion,
        "dbSystem" .= ASV.getValidDbSystem spec,
        "decls" .= AS.decls spec
      ]

instance Inspectable InspectableAppSpec where
  inspect (InspectableAppSpec spec) =
    InspectionEntry
      "Spec"
      [ ("Version", show waspVersion),
        ("Database", show (ASV.getValidDbSystem spec))
      ]
      : concatMap inspect (AS.decls spec)
