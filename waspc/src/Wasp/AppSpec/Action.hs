{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.Action
  ( Action (..),
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

data Action = Action
  { fn :: ExtImport,
    entities :: Maybe [Ref Entity],
    auth :: Maybe Bool
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

instance IsDecl Action

instance Inspectable Action where
  inspect action =
    [ InspectionEntry "Actions" $
        [("Import", showExtImportFromProjectDir $ fn action)]
          ++ [("Entities", (intercalate ", " . fmap refName) entities') | Just entities' <- [entities action]]
          ++ [("Auth", "Enabled") | auth action == Just True]
    ]
