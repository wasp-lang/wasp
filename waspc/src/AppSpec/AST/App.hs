{-# LANGUAGE DeriveDataTypeable #-}

module AppSpec.AST.App (App (..)) where

import AppSpec.AST.AuthMethod (AuthMethod)
import AppSpec.AST.Page (Page)
import AppSpec.AST.Ref (Ref)
import Data.Data (Data)

data App = App
  { title :: String,
    authMethod :: AuthMethod,
    defaultPage :: Ref Page
  }
  deriving (Show, Eq, Data)
