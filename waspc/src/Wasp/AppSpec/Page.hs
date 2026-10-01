{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.Page
  ( Page (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import GHC.Generics (Generic)
import Wasp.AppSpec.Core.IsDecl (IsDecl (..))
import Wasp.AppSpec.ExtImport (ExtImport, showExtImportFromProjectDir)
import Wasp.Inspectable (Inspectable (..), InspectionEntry (InspectionEntry))

data Page = Page
  { name :: String,
    component :: ExtImport,
    authRequired :: Maybe Bool
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

instance IsDecl Page where
  declName = name

instance Inspectable Page where
  inspect page =
    [ InspectionEntry "Pages" $
        ("Import", showExtImportFromProjectDir $ component page)
          : [("Requires auth", "Yes") | authRequired page == Just True]
    ]
