{-# LANGUAGE DeriveDataTypeable #-}

module AppSpec.App (App (..)) where

import AppSpec.AuthMethod (AuthMethod)
import AppSpec.Core.Decl (IsDecl)
import AppSpec.Core.Ref (Ref)
import AppSpec.Page (Page)
import Data.Data (Data)

data App = App
  { title :: String,
    authMethod :: AuthMethod,
    defaultPage :: Ref Page
  }
  deriving (Show, Eq, Data)

instance IsDecl App
