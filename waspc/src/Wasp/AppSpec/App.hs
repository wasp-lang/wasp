{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.App (App (..)) where

import Data.Data (Data)
import Wasp.AppSpec.App.Auth (Auth)
import Wasp.AppSpec.Core.Decl (IsDecl)

data App = App
  { title :: String,
    head :: Maybe [String],
    auth :: Maybe Auth -- NOTE: This is new. Before, `auth` was a standalone declaration.
  }
  deriving (Show, Eq, Data)

instance IsDecl App
