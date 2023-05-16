{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.App.WebSocket
  ( WebSocket (..),
  )
where

import Data.Data (Data)
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.Entity (Entity)
import Wasp.AppSpec.ExtImport (ExtImport)

data WebSocket = WebSocket
  { fn :: ExtImport,
    entities :: Maybe [Ref Entity]
  }
  deriving (Show, Eq, Data)
