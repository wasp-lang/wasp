{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.JSON
  ( JSON (..),
  )
where

import Data.Data (Data)

newtype JSON = JSON String
  deriving (Show, Eq, Data)
