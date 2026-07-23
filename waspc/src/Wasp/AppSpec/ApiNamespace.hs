{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.ApiNamespace
  ( ApiNamespace (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import GHC.Generics (Generic)
import Wasp.AppSpec.Core.Inspectable (Inspectable (..), InspectionEntry (InspectionEntry))
import Wasp.AppSpec.Core.IsDecl (IsDecl)
import Wasp.AppSpec.ExtImport (ExtImport, showExtImportFromProjectDir)

data ApiNamespace = ApiNamespace
  { middlewareConfigFn :: ExtImport,
    path :: String
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

instance IsDecl ApiNamespace

instance Inspectable ApiNamespace where
  inspect apiNamespace =
    [ InspectionEntry
        "API namespaces"
        [ ("Path", path apiNamespace),
          ("Import", showExtImportFromProjectDir $ middlewareConfigFn apiNamespace)
        ]
    ]
