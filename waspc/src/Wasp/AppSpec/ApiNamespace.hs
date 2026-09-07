{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.ApiNamespace
  ( ApiNamespace (..),
    ignoresServerBasePath,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import GHC.Generics (Generic)
import Wasp.AppSpec.Core.IsDecl (IsDecl)
import Wasp.AppSpec.ExtImport (ExtImport, showExtImportFromProjectDir)
import Wasp.Inspectable (Inspectable (..), InspectionEntry (InspectionEntry))

data ApiNamespace = ApiNamespace
  { middlewareConfigFn :: ExtImport,
    path :: String,
    ignoreServerBasePath :: Maybe Bool
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

instance IsDecl ApiNamespace

instance Inspectable ApiNamespace where
  inspect apiNamespace =
    [ InspectionEntry
        "API namespaces"
        ( [ ("Path", path apiNamespace),
            ("Import", showExtImportFromProjectDir $ middlewareConfigFn apiNamespace)
          ]
            ++ [("Ignores server base path", "Yes") | ignoresServerBasePath apiNamespace]
        )
    ]

-- | Whether the namespace applies to its path on the server's origin root instead of under the server base path.
ignoresServerBasePath :: ApiNamespace -> Bool
ignoresServerBasePath = (== Just True) . ignoreServerBasePath
