{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.App.Auth.EmailVerification where

import Data.Data (Data)
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.ExtImport (ExtImport)
import Wasp.AppSpec.Route (Route)

data EmailVerificationConfig = EmailVerificationConfig
  { getEmailContentFn :: Maybe ExtImport,
    onVerifySuccessRedirectTo :: Ref Route
  }
  deriving (Show, Eq, Data)
