{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Where an operation is available.
module Wasp.AppSpec.Expose
  ( Expose (..),
    isExposedToClient,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)

data Expose = Expose
  { -- | Whether the operation gets an HTTP route and a client export. Defaults to true.
    client :: Maybe Bool
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)

-- | Operations get an HTTP route and a client export unless they opt out.
isExposedToClient :: Maybe Expose -> Bool
isExposedToClient = maybe True (fromMaybe True . client)
