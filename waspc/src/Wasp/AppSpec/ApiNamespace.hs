{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.ApiNamespace
  ( ApiNamespace (..),
  )
where

import Data.Data (Data)
import Wasp.AppSpec.Core.Decl (IsDecl)
import Wasp.AppSpec.ExtImport (ExtImport)

data ApiNamespace = ApiNamespace
  { middlewareConfigFn :: ExtImport,
    path :: String
  }
  deriving (Show, Eq, Data)

instance IsDecl ApiNamespace
