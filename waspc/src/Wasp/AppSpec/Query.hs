{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.Query
  ( Query (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import GHC.Generics (Generic)
import Wasp.AppSpec.Core.Inspectable (Inspectable (..), InspectionEntry (..))
import Wasp.AppSpec.Core.IsDecl (IsDecl)
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.Entity
import Wasp.AppSpec.ExtImport

data Query = Query
  { fn :: ExtImport,
    entities :: Maybe [Ref Entity],
    auth :: Maybe Bool
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

instance IsDecl Query

instance Inspectable Query where
  inspectionSection = "Queries"
  inspect (name, query) =
    InspectionEntry
      [ name,
        if auth query == Just True then "[auth]" else "",
        showExtImport $ fn query,
        showEntityRefs $ entities query
      ]
