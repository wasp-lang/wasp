{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.Query
  ( Query (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.List (intercalate)
import GHC.Generics (Generic)
import Wasp.AppSpec.Core.Inspectable (Inspectable (..), InspectionEntry (InspectionEntry))
import Wasp.AppSpec.Core.IsDecl (IsDecl)
import Wasp.AppSpec.Core.Ref (Ref, refName)
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
  inspect query =
    [ InspectionEntry "Queries" $
        [("Import", showExtImport $ fn query)]
          ++ [("Entities", (intercalate ", " . fmap refName) entities') | Just entities' <- [entities query]]
          ++ [("Auth", "Enabled") | auth query == Just True]
    ]
