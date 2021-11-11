{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.App (App (..)) where

import Data.Data (Data)
import Wasp.AppSpec.AuthMethod (AuthMethod)
import Wasp.AppSpec.Core.Decl (IsDecl)
import Wasp.AppSpec.Core.Ref (Ref)
import Wasp.AppSpec.Page (Page)

data App = App
  { title :: String,
    authMethod :: AuthMethod,
    defaultPage :: Ref Page
  }
  deriving (Show, Eq, Data)

instance IsDecl App
