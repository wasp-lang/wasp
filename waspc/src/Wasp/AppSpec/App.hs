{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.App (App (..)) where

import Data.Data (Data)
import Wasp.AppSpec.App.Auth (Auth)
import Wasp.AppSpec.Core.Decl (IsDecl)
import Wasp.AppSpec.JSON (JSON)

data App = App
  { title :: String,
    head :: Maybe [String],
    auth :: Maybe Auth, -- NOTE: This is new. Before, `auth` was a standalone declaration.

    -- | TODO: In current Wasp, we have a more sophisticated AST here: not just JSON, but [NpmDependency].
    -- We should look into this here, figure out how we can go about it and also have such more advanced
    -- representation here. Question is, where does this parsing from JSON to [NpmDependency] happen?
    -- Or is it just Dependency? Should we have NpmDependencies below instead of Dependencies?
    dependencies :: Maybe JSON -- NOTE: This is new. Before, `auth` was a standalone declaration.
  }
  deriving (Show, Eq, Data)

instance IsDecl App
