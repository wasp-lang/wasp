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
    -- TODO: We might want to look into making this nicer in the future
    -- -> maybe change how it is represented in the wasp lang, e.g. make it just a list of strings,
    -- while here it would still be what it is or it would be parsed even more.
    -- We could make this work by supporting manual definition of how Dependency evaluates / what is its type.
    -- So, similar like we have IsDeclType instance for declarations, we could have similar abstraction for
    -- sub-types like these, and we would inject it into Analyzer and it would be defined manually.
    --   TODO: Make a github issue for this (TODO above)?
    dependencies :: Maybe [Dependency]
  }
  deriving (Show, Eq, Data)

instance IsDecl App
