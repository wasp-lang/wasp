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
import Wasp.AppSpec.Core.Inspectable (Inspectable (..), InspectionEntry (..))
import Wasp.AppSpec.Core.IsDecl (IsDecl)
import Wasp.AppSpec.ExtImport (ExtImport, showExtImport)

data Page = Page
  { component :: ExtImport,
    authRequired :: Maybe Bool
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

instance IsDecl Page

instance Inspectable Page where
  inspectionSection = "Pages"
  inspect (name, page) =
    InspectionEntry
      [ name,
        if authRequired page == Just True then "[auth]" else "",
        showExtImport $ component page
      ]
