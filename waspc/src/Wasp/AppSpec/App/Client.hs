{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.App.Client
  ( Client (..),
  )
where

import Data.Data (Data)
import Wasp.AppSpec.ExtImport (ExtImport)

data Client = Client
  { setupFn :: Maybe ExtImport,
    rootComponent :: Maybe ExtImport,
    baseDir :: Maybe String
  }
  deriving (Show, Eq, Data)
