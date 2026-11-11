{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.App.Server
  ( Server (..),
    openApiDocumentPath,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import GHC.Generics (Generic)
import Wasp.AppSpec.ExtImport (ExtImport)

data Server = Server
  { setupFn :: Maybe ExtImport,
    middlewareConfigFn :: Maybe ExtImport,
    envValidationSchema :: Maybe ExtImport,
    traceDeps :: Maybe [String],
    openApi :: Maybe Bool
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

-- | Path at which the server serves the OpenAPI document when `openApi` is enabled.
openApiDocumentPath :: String
openApiDocumentPath = "/openapi.json"
