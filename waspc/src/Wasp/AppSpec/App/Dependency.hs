{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.App.Dependency
  ( Dependency (..),
  )
where

import Data.Data (Data)

data Dependency = Dependency
  { name :: String,
    version :: String
  }
  deriving (Show, Eq, Data)
