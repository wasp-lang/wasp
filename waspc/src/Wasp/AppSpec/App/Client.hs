{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.App.Client
  ( Client (..),
  )
where

import qualified Data.Aeson as Aeson
import Data.Data (Data)
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Wasp.AppSpec.ExtImport (ExtImport)
import Wasp.AppSpec.JSON (maybeToField)

data Client = Client
  { setupFn :: Maybe ExtImport,
    rootComponent :: Maybe ExtImport,
    -- We expect the base dir to start with a slash e.g. /client
    baseDir :: Maybe String,
    envValidationSchema :: Maybe ExtImport
  }
  deriving (Show, Eq, Data, Generic, Aeson.FromJSON)

instance Aeson.ToJSON Client where
  toJSON client =
    let optionalFields =
          [ maybeToField "setupFn" (setupFn client),
            maybeToField "rootComponent" (rootComponent client),
            maybeToField "baseDir" (baseDir client),
            maybeToField "envValidationSchema" (envValidationSchema client)
          ]
     in Aeson.object (catMaybes optionalFields)

