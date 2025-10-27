{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.App.Server
  ( Server (..),
  )
where

import qualified Data.Aeson as Aeson
import Data.Data (Data)
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Wasp.AppSpec.ExtImport (ExtImport)
import Wasp.AppSpec.JSON (maybeToField)

data Server = Server
  { setupFn :: Maybe ExtImport,
    middlewareConfigFn :: Maybe ExtImport,
    envValidationSchema :: Maybe ExtImport
  }
  deriving (Show, Eq, Data, Generic, Aeson.FromJSON)

instance Aeson.ToJSON Server where
  toJSON server =
    let optionalFields =
          [ maybeToField "setupFn" (setupFn server),
            maybeToField "middlewareConfigFn" (middlewareConfigFn server),
            maybeToField "envValidationSchema" (envValidationSchema server)
          ]
     in Aeson.object (catMaybes optionalFields)
