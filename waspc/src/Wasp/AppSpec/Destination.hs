{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.Destination
  ( Destination (..),
    DestinationKind (..),
    isApiDestination,
  )
where

import Data.Aeson (FromJSON)
import Data.Data (Data)
import GHC.Generics (Generic)

-- | A place the app can send users to: a client-side route or a server-side
-- API endpoint. The path is flattened from the referenced Route or Api decl.
data Destination = Destination
  { kind :: DestinationKind,
    path :: String
  }
  deriving (Show, Eq, Data, Generic, FromJSON)

data DestinationKind = Route | Api
  deriving (Show, Eq, Data, Generic, FromJSON)

isApiDestination :: Destination -> Bool
isApiDestination destination = kind destination == Api
