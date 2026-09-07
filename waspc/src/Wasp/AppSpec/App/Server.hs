{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.App.Server
  ( Server (..),
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
    -- | Absolute URL path under which the whole server is mounted: Wasp's own routes
    -- (auth, operations, crud), user apis and routes added in `setupFn`. Defaults to "/" when not set.
    basePath :: Maybe String
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)
