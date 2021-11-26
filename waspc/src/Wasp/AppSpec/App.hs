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
    auth :: Maybe Auth, -- NOTE: This is new. Before, `auth` was a standalone declaration.
    server :: Maybe Server, -- NOTE: This is new. Before, `server` was a standalone declaration.
    db :: Maybe Db, -- NOTE: This is new. Before, `db` was a standalone declaration.

    -- | NOTE: This is new. Before, `dependencies` was a standalone declaration and it was a {=json json=},
    -- while now it is a [{ name :: String, version :: String }].
    dependencies :: Maybe [Dependency]
  }
  deriving (Show, Eq, Data)

instance IsDecl App
