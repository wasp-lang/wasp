{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.App.Wasp (Wasp (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import GHC.Generics (Generic)

data Wasp = Wasp
  { version :: String
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)
