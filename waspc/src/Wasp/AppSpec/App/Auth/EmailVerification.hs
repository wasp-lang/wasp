{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.AppSpec.App.Auth.EmailVerification where

import Data.Aeson (FromJSON)
import Data.Data (Data)
import GHC.Generics (Generic)
import Wasp.AppSpec.ExtImport (ExtImport)

data EmailVerificationConfig = EmailVerificationConfig
  { getEmailContentFn :: Maybe ExtImport,
    clientRoute :: String
  }
  deriving (Show, Eq, Data, Generic, FromJSON)
