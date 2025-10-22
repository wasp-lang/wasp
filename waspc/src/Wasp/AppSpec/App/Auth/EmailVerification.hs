{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.App.Auth.EmailVerification where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import GHC.Generics (Generic)
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.ExtImport (ExtImport)
import Wasp.AppSpec.Route (Route)

data EmailVerificationConfig = EmailVerificationConfig
  { getEmailContentFn :: Maybe ExtImport,
    clientRoute :: Ref Route
  }
  deriving (Show, Eq, Data, Generic, FromJSON, ToJSON)
