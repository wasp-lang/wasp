{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.App.Auth.PasswordReset where

import Data.Data (Data)
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.ExtImport (ExtImport)
import Wasp.AppSpec.Route (Route)

data PasswordResetConfig = PasswordResetConfig
  { getEmailContentFn :: Maybe ExtImport,
    clientRoute :: Ref Route
  }
  deriving (Show, Eq, Data)
