{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.App.Client
  ( Client (..),
  )
where

import Data.Aeson (FromJSON)
import Data.Data (Data)
import GHC.Generics (Generic)
import Wasp.AppSpec.ExtImport (ExtImport)

data Client = Client
  { setupFn :: Maybe ExtImport,
    rootComponent :: Maybe ExtImport,
    -- We expect the base dir to start with a slash e.g. /client
    baseDir :: Maybe String,
    envValidationFn :: Maybe ExtImport
  }
  deriving (Show, Eq, Data, Generic, FromJSON)
