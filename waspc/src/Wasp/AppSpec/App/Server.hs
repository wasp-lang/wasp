{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.App.Server
  ( Server (..),
  )
where

import Data.Data (Data)
import Wasp.AppSpec.ExtImport (ExtImport)

data Server = Server
  { setupFn :: Maybe ExtImport,
    middlewareConfigFn :: Maybe ExtImport
  }
  deriving (Show, Eq, Data)
