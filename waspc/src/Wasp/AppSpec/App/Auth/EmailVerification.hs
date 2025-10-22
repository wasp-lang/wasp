{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.App.Auth.EmailVerification where

import qualified Data.Aeson as Aeson
import Data.Data (Data)
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.ExtImport (ExtImport)
import Wasp.AppSpec.JSON (maybeToField)
import Wasp.AppSpec.Route (Route)

data EmailVerificationConfig = EmailVerificationConfig
  { getEmailContentFn :: Maybe ExtImport,
    clientRoute :: Ref Route
  }
  deriving (Show, Eq, Data, Generic, Aeson.FromJSON)

instance Aeson.ToJSON EmailVerificationConfig where
  toJSON config =
    let requiredFields = ["clientRoute" Aeson..= clientRoute config]
        optionalFields =
          [ maybeToField "getEmailContentFn" (getEmailContentFn config)
          ]
     in Aeson.object (requiredFields <> catMaybes optionalFields)
