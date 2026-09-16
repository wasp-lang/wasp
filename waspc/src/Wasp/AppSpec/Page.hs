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
import Wasp.AppSpec.AuthRequirement (AuthRequirement, isAuthRequiredWithDefault)
import Wasp.AppSpec.Core.IsDecl (IsDecl)
import Wasp.AppSpec.ExtImport (ExtImport, showExtImportFromProjectDir)
import Wasp.Inspectable (Inspectable (..), InspectionEntry (InspectionEntry))

data Page = Page
  { component :: ExtImport,
    authRequired :: Maybe AuthRequirement
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

instance IsDecl Page

instance Inspectable Page where
  inspect page =
    [ InspectionEntry "Pages" $
        ("Import", showExtImportFromProjectDir $ component page)
          : [("Requires auth", "Yes") | isAuthRequiredWithDefault False (authRequired page)]
    ]
