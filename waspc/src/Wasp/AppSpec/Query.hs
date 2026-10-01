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
import Wasp.AppSpec.Core.IsDecl (IsDecl (..))
import Wasp.AppSpec.Core.Ref (Ref, refName)
import Wasp.AppSpec.Entity (Entity)
import Wasp.AppSpec.ExtImport (ExtImport, showExtImportFromProjectDir)
import Wasp.Inspectable (Inspectable (..), InspectionEntry (InspectionEntry))

data Query = Query
  { name :: String,
    fn :: ExtImport,
    entities :: Maybe [Ref Entity],
    auth :: Maybe Bool
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

instance IsDecl Query where
  declName = name

instance Inspectable Query where
  inspect query =
    [ InspectionEntry "Queries" $
        [ ("Name", name query),
          ("Import", showExtImportFromProjectDir $ fn query)
        ]
          ++ [("Entities", (intercalate ", " . fmap refName) entities') | Just entities' <- [entities query]]
          ++ [("Auth", "Enabled") | auth query == Just True]
    ]
