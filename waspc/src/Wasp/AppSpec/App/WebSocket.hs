{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Wasp.AppSpec.App.WebSocket
  ( WebSocket (..),
  )
where

import qualified Data.Aeson as Aeson
import Data.Data (Data)
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Wasp.AppSpec.ExtImport (ExtImport)
import Wasp.AppSpec.JSON (maybeToField)

data WebSocket = WebSocket
  { fn :: ExtImport,
    autoConnect :: Maybe Bool
  }
  deriving (Show, Eq, Data, Generic, Aeson.FromJSON)

instance Aeson.ToJSON WebSocket where
  toJSON :: WebSocket -> Aeson.Value
  toJSON webSocket =
    let requiredFields = ["fn" Aeson..= fn webSocket]
        optionalFields =
          [ maybeToField "autoConnect" (autoConnect webSocket)
          ]
     in Aeson.object (requiredFields <> catMaybes optionalFields)
