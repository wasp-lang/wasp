{-# LANGUAGE DeriveDataTypeable #-}

module AppSpec.AST.Domain.App (App (..)) where

import AppSpec.AST.Core.Ref (Ref)
import AppSpec.AST.Domain.AuthMethod (AuthMethod)
import AppSpec.AST.Domain.Page (Page)
import Data.Data (Data)

data App = App
  { title :: String,
    authMethod :: AuthMethod,
    defaultPage :: Ref Page
  }
  deriving (Show, Eq, Data)
