{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.App (App (..)) where

import Data.Data (Data)
import Wasp.AppSpec.App.Auth (Auth)
import Wasp.AppSpec.App.Db (Db)
import Wasp.AppSpec.App.Dependency (Dependency)
import Wasp.AppSpec.App.Server (Server)
import Wasp.AppSpec.Core.Decl (IsDecl)

data App = App
  { title :: String,
    head :: Maybe [String],
    auth :: Maybe Auth,
    server :: Maybe Server,
    db :: Maybe Db,
    dependencies :: Maybe [Dependency]
  }
  deriving (Show, Eq, Data)

instance IsDecl App
