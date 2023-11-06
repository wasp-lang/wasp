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
    -- We expect the base dir to start with a slash e.g. /client
    baseDir :: Maybe String
  }
  deriving (Show, Eq, Data)
